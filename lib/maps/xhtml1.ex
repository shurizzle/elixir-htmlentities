use HTMLEntities.ConversionTable

defconversions :xhtml1 do
  mod = Module.concat(HTMLEntities.ConversionTable, :html4)

  def convert("apos"), do: << 39 :: utf8 >>
  defdelegate convert(entity), to: mod

  def revert(<< 39 :: utf8 >>), do: "apos"
  defdelegate revert(char), to: mod

  defdelegate entity_pattern, to: mod
end
