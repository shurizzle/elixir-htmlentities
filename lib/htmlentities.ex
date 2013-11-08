defmodule HTMLEntities do
  @instructions [:basic, :named, :decimal, :hexadecimal]

  defmacrop build_basic_entity_encoder do
    quote do
      cond do
        Enum.member?(var!(instructions), :basic) or Enum.member?(var!(instructions), :named) ->
          fn(match) ->
            encode_named(var!(map), Enum.at(match, 0))
          end

        Enum.member?(var!(instructions), :decimal) ->
          fn(match) ->
            encode_decimal(Enum.at(match, 0))
          end

        Enum.member?(var!(instructions), :hexadecimal) ->
          fn(match) ->
            encode_hexadecimal(Enum.at(match, 0))
          end
      end
    end
  end

  @spec encode(String.t) :: { :ok, String.t } | { :error, atom }
  def encode(source) do
    encode(source, :xhtml1, [:basic])
  end

  @spec encode(String.t, atom | [atom]) :: { :ok, String.t } | { :error, atom }
  def encode(source, map) when is_atom(map) do
    encode(source, map, [:basic])
  end

  def encode(source, instructions) when is_list(instructions) do
    encode(source, :xhtml1, instructions)
  end

  @spec encode(String.t, atom, [atom]) :: { :ok, String.t } | { :error, atom }
  def encode(source, map, instructions) do
    if Enum.empty?(instructions -- @instructions) do
      if Enum.member?(instructions, :decimal) and Enum.member?(instructions, :hexadecimal) do
        { :error, :conflictual_instructions }
      else
        map = __MODULE__.ConversionTable.get(map)
        encode_basic = build_basic_entity_encoder
        operations = intersect([:named, :decimal, :hexadecimal], instructions)
        encode_extended = fn(match) ->
          extended_encoder(operations, map, Enum.at(match, 0))
        end

        { :ok, replace(replace(source, map.basic_entity, encode_basic), map.extended_entity, encode_extended) }
      end
    else
      { :error, :invalid_instructions }
    end
  end

  @spec decode(String.t) :: String.t
  @spec decode(String.t, atom) :: String.t
  def decode(source, map // :xhtml1) when is_binary(source) do
    map = __MODULE__.ConversionTable.get(map)

    replace(source,
      %r/&(?:(#{map.entity_pattern})|#([0-9]{1,7})|#x([0-9a-f]{1,6}));/i,
      fn(matches) ->
        first = map.convert(Enum.at(matches, 1))
        if first do
          first
        else
          second = Enum.at(matches, 2)
          if second do
            binary_to_integer(second, 10)
          else
            third = Enum.at(matches, 3)
            if third do
              binary_to_integer(third, 16)
            else
              Enum.at(matches, 0)
            end
          end
        end
      end)
  end

  defp replace(string, regex, opts // [], fun) do
    opts = Keyword.put opts, :return, :index
    opts = Keyword.delete opts, :capture
    replace(string, regex, opts, fun, <<>>)
  end

  defp replace(nil, _, _, _, res), do: res
  defp replace(<<>>, _, _, _, res), do: res

  defp replace(rest, regex, opts, fun, res) do
    case Regex.run(regex, rest, opts) do
      nil ->
        res <> rest

      captures when is_list(captures) ->
        pieces = Enum.map captures, fn({ start, stop }) ->
          binary_part(rest, start, stop)
        end

        case fun.(pieces) do
          replace when is_binary(replace) ->
            [{ start, stop }|_] = captures
            first = binary_part(rest, 0, start)
            rest = binary_part(rest, start + stop, byte_size(rest) - start - stop)
            replace(rest, regex, opts, fun, res <> first <> replace)

          _ ->
            { :error, :invalid_replace }
        end
    end
  end

  defp intersect(list1, list2) when is_list(list1) and is_list(list2) do
    Enum.reject list1, &(!&1 in list2)
  end

  defp extended_encoder([], _, char), do: char

  defp extended_encoder([:named|ops], map, char) do
    encoded = encode_named(map, char)

    if nil?(encoded) or encoded == char do
      extended_encoder(ops, map, char)
    else
      encoded
    end
  end

  defp extended_encoder([:decimal|ops], map, char) do
    encoded = encode_decimal(char)

    if nil?(encoded) or encoded == char do
      extended_encoder(ops, map, char)
    else
      encoded
    end
  end

  defp extended_encoder([:hexadecimal|ops], map, char) do
    encoded = encode_hexadecimal(char)

    if nil?(encoded) or encoded == char do
      extended_encoder(ops, map, char)
    else
      encoded
    end
  end

  defp encode_named(map, char) do
    e = map.revert(char)

    if e do
      "&#{e};"
    else
      char
    end
  end

  defp encode_decimal(<< char :: utf8 >>) do
    "&##{integer_to_binary(char, 10)};"
  end

  defp encode_hexadecimal(<< char :: utf8 >>) do
    "&#x#{integer_to_binary(char, 16)};"
  end

  defmodule ConversionTable do
    @doc nil
    defmacro __using__(_) do
      quote do
        import unquote(__MODULE__), only: [defconversions: 2]
      end
    end

    defmacro create_lengths do
      quote unquote: false do
        if nil?(@key_length_min) or nil?(@key_length_max) do
          lengths = Enum.map @map, fn({ name, _ }) ->
            String.length(name)
          end

          Module.put_attribute __MODULE__, :key_length_min, Enum.min(lengths)
          Module.put_attribute __MODULE__, :key_length_max, Enum.max(lengths)
        end
      end
    end

    defmacro key_length_min do
      quote do
        unquote(__MODULE__).create_lengths
        @key_length_min
      end
    end

    defmacro key_length_max do
      quote do
        unquote(__MODULE__).create_lengths
        @key_length_max
      end
    end

    defmacro defentity(pattern) do
      quote bind_quoted: [pattern: pattern] do
        if is_record(pattern, Regex) do
          def entity_pattern, do: unquote(Regex.source(pattern))
        else
          def entity_pattern, do: unquote(pattern)
        end
      end
    end

    defmacro defbasic(pattern) do
      quote bind_quoted: [pattern: pattern] do
        if is_record(pattern, Regex) do
          def basic_entity, do: unquote(Macro.escape(pattern))
        else
          def basic_entity, do: unquote(Macro.escape(%r/#{pattern}/))
        end
      end
    end

    defmacro defextended(pattern) do
      quote bind_quoted: [pattern: pattern] do
        if is_record(pattern, Regex) do
          def extended_entity, do: unquote(Macro.escape(pattern))
        else
          def extended_entity, do: unquote(Macro.escape(%r/#{pattern}/))
        end
      end
    end

    defmacro defconversions(name, do: body) do
      quote do
        defmodule Module.concat(unquote(__MODULE__), unquote(name)) do
          import unquote(__MODULE__), only: [defentity: 1, defbasic: 1, defextended: 1, key_length_min: 0, key_length_max: 0]
          @before_compile unquote(__MODULE__)

          @map nil
          @skip_dup_encodings nil
          @key_length_min nil
          @key_length_max nil

          defbasic %r/[<>"'&]/
          defextended %r/[^\x{20}-\x{7E}]/

          defoverridable basic_entity: 0, extended_entity: 0

          unquote(body)
        end
      end
    end

    @doc nil
    defmacro __before_compile__(_) do
      quote unquote: false do
        if is_list(@map) do
          quote do
            unquote Enum.map @map, fn({ name, value }) ->
              def convert(unquote(name)), do: unquote(value)
            end
          end
        end
        def convert(_), do: nil


        if is_list(@map) do
          skips = if is_list(@skip_dup_encodings), do: @skip_dup_encodings, else: []
          valids = Enum.reject @map, fn({ name, _ }) ->
            Enum.member?(skips, name)
          end
          valids = Enum.uniq valids, fn({ _, value }) ->
            value
          end

          quote do
            unquote Enum.map valids, fn({ name, value }) ->
              def revert(unquote(value)), do: unquote(name)
            end
          end
        end
        def revert(_), do: nil


        if is_list(@map) and not Module.defines?(__MODULE__, { :entity_pattern, 0 }, :def) do
          if Module.defines?(__MODULE__, { :entity_pattern, 0 }) do
            defoverridable entity_pattern: 0
          end

          defentity %r/[a-z][a-z0-9]{#{key_length_min - 1},#{key_length_max - 1}}/
        end

        Module.delete_attribute(__MODULE__, :map)
        Module.delete_attribute(__MODULE__, :skip_dup_encodings)
        Module.delete_attribute(__MODULE__, :key_length_min)
        Module.delete_attribute(__MODULE__, :key_length_max)
      end
    end

    def get(map) do
      Module.safe_concat(HTMLEntities.ConversionTable, map)
    end
  end
end
