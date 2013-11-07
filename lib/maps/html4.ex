use HTMLEntities.Map

defmap :html4 do
  @map [
    { "Aacute", << 193 :: utf8 >> },
    { "aacute", << 225 :: utf8 >> },
    { "Acirc", << 194 :: utf8 >> },
    { "acirc", << 226 :: utf8 >> },
    { "acute", << 180 :: utf8 >> },
    { "AElig", << 198 :: utf8 >> },
    { "aelig", << 230 :: utf8 >> },
    { "Agrave", << 192 :: utf8 >> },
    { "agrave", << 224 :: utf8 >> },
    { "alefsym", << 8501 :: utf8 >> },
    { "Alpha", << 913 :: utf8 >> },
    { "alpha", << 945 :: utf8 >> },
    { "amp", << 38 :: utf8 >> },
    { "and", << 8743 :: utf8 >> },
    { "ang", << 8736 :: utf8 >> },
    { "Aring", << 197 :: utf8 >> },
    { "aring", << 229 :: utf8 >> },
    { "asymp", << 8776 :: utf8 >> },
    { "Atilde", << 195 :: utf8 >> },
    { "atilde", << 227 :: utf8 >> },
    { "Auml", << 196 :: utf8 >> },
    { "auml", << 228 :: utf8 >> },
    { "bdquo", << 8222 :: utf8 >> },
    { "Beta", << 914 :: utf8 >> },
    { "beta", << 946 :: utf8 >> },
    { "brvbar", << 166 :: utf8 >> },
    { "bull", << 8226 :: utf8 >> },
    { "cap", << 8745 :: utf8 >> },
    { "Ccedil", << 199 :: utf8 >> },
    { "ccedil", << 231 :: utf8 >> },
    { "cedil", << 184 :: utf8 >> },
    { "cent", << 162 :: utf8 >> },
    { "Chi", << 935 :: utf8 >> },
    { "chi", << 967 :: utf8 >> },
    { "circ", << 710 :: utf8 >> },
    { "clubs", << 9827 :: utf8 >> },
    { "cong", << 8773 :: utf8 >> },
    { "copy", << 169 :: utf8 >> },
    { "crarr", << 8629 :: utf8 >> },
    { "cup", << 8746 :: utf8 >> },
    { "curren", << 164 :: utf8 >> },
    { "Dagger", << 8225 :: utf8 >> },
    { "dagger", << 8224 :: utf8 >> },
    { "dArr", << 8659 :: utf8 >> },
    { "darr", << 8595 :: utf8 >> },
    { "deg", << 176 :: utf8 >> },
    { "Delta", << 916 :: utf8 >> },
    { "delta", << 948 :: utf8 >> },
    { "diams", << 9830 :: utf8 >> },
    { "divide", << 247 :: utf8 >> },
    { "Eacute", << 201 :: utf8 >> },
    { "eacute", << 233 :: utf8 >> },
    { "Ecirc", << 202 :: utf8 >> },
    { "ecirc", << 234 :: utf8 >> },
    { "Egrave", << 200 :: utf8 >> },
    { "egrave", << 232 :: utf8 >> },
    { "empty", << 8709 :: utf8 >> },
    { "emsp", << 8195 :: utf8 >> },
    { "ensp", << 8194 :: utf8 >> },
    { "Epsilon", << 917 :: utf8 >> },
    { "epsilon", << 949 :: utf8 >> },
    { "equiv", << 8801 :: utf8 >> },
    { "Eta", << 919 :: utf8 >> },
    { "eta", << 951 :: utf8 >> },
    { "ETH", << 208 :: utf8 >> },
    { "eth", << 240 :: utf8 >> },
    { "Euml", << 203 :: utf8 >> },
    { "euml", << 235 :: utf8 >> },
    { "euro", << 8364 :: utf8 >> },
    { "exist", << 8707 :: utf8 >> },
    { "fnof", << 402 :: utf8 >> },
    { "forall", << 8704 :: utf8 >> },
    { "frac12", << 189 :: utf8 >> },
    { "frac14", << 188 :: utf8 >> },
    { "frac34", << 190 :: utf8 >> },
    { "frasl", << 8260 :: utf8 >> },
    { "Gamma", << 915 :: utf8 >> },
    { "gamma", << 947 :: utf8 >> },
    { "ge", << 8805 :: utf8 >> },
    { "gt", << 62 :: utf8 >> },
    { "hArr", << 8660 :: utf8 >> },
    { "harr", << 8596 :: utf8 >> },
    { "hearts", << 9829 :: utf8 >> },
    { "hellip", << 8230 :: utf8 >> },
    { "Iacute", << 205 :: utf8 >> },
    { "iacute", << 237 :: utf8 >> },
    { "Icirc", << 206 :: utf8 >> },
    { "icirc", << 238 :: utf8 >> },
    { "iexcl", << 161 :: utf8 >> },
    { "Igrave", << 204 :: utf8 >> },
    { "igrave", << 236 :: utf8 >> },
    { "image", << 8465 :: utf8 >> },
    { "infin", << 8734 :: utf8 >> },
    { "int", << 8747 :: utf8 >> },
    { "Iota", << 921 :: utf8 >> },
    { "iota", << 953 :: utf8 >> },
    { "iquest", << 191 :: utf8 >> },
    { "isin", << 8712 :: utf8 >> },
    { "Iuml", << 207 :: utf8 >> },
    { "iuml", << 239 :: utf8 >> },
    { "Kappa", << 922 :: utf8 >> },
    { "kappa", << 954 :: utf8 >> },
    { "Lambda", << 923 :: utf8 >> },
    { "lambda", << 955 :: utf8 >> },
    { "lang", << 9001 :: utf8 >> },
    { "laquo", << 171 :: utf8 >> },
    { "lArr", << 8656 :: utf8 >> },
    { "larr", << 8592 :: utf8 >> },
    { "lceil", << 8968 :: utf8 >> },
    { "ldquo", << 8220 :: utf8 >> },
    { "le", << 8804 :: utf8 >> },
    { "lfloor", << 8970 :: utf8 >> },
    { "lowast", << 8727 :: utf8 >> },
    { "loz", << 9674 :: utf8 >> },
    { "lrm", << 8206 :: utf8 >> },
    { "lsaquo", << 8249 :: utf8 >> },
    { "lsquo", << 8216 :: utf8 >> },
    { "lt", << 60 :: utf8 >> },
    { "macr", << 175 :: utf8 >> },
    { "mdash", << 8212 :: utf8 >> },
    { "micro", << 181 :: utf8 >> },
    { "middot", << 183 :: utf8 >> },
    { "minus", << 8722 :: utf8 >> },
    { "Mu", << 924 :: utf8 >> },
    { "mu", << 956 :: utf8 >> },
    { "nabla", << 8711 :: utf8 >> },
    { "nbsp", << 160 :: utf8 >> },
    { "ndash", << 8211 :: utf8 >> },
    { "ne", << 8800 :: utf8 >> },
    { "ni", << 8715 :: utf8 >> },
    { "not", << 172 :: utf8 >> },
    { "notin", << 8713 :: utf8 >> },
    { "nsub", << 8836 :: utf8 >> },
    { "Ntilde", << 209 :: utf8 >> },
    { "ntilde", << 241 :: utf8 >> },
    { "Nu", << 925 :: utf8 >> },
    { "nu", << 957 :: utf8 >> },
    { "Oacute", << 211 :: utf8 >> },
    { "oacute", << 243 :: utf8 >> },
    { "Ocirc", << 212 :: utf8 >> },
    { "ocirc", << 244 :: utf8 >> },
    { "OElig", << 338 :: utf8 >> },
    { "oelig", << 339 :: utf8 >> },
    { "Ograve", << 210 :: utf8 >> },
    { "ograve", << 242 :: utf8 >> },
    { "oline", << 8254 :: utf8 >> },
    { "Omega", << 937 :: utf8 >> },
    { "omega", << 969 :: utf8 >> },
    { "Omicron", << 927 :: utf8 >> },
    { "omicron", << 959 :: utf8 >> },
    { "oplus", << 8853 :: utf8 >> },
    { "or", << 8744 :: utf8 >> },
    { "ordf", << 170 :: utf8 >> },
    { "ordm", << 186 :: utf8 >> },
    { "Oslash", << 216 :: utf8 >> },
    { "oslash", << 248 :: utf8 >> },
    { "Otilde", << 213 :: utf8 >> },
    { "otilde", << 245 :: utf8 >> },
    { "otimes", << 8855 :: utf8 >> },
    { "Ouml", << 214 :: utf8 >> },
    { "ouml", << 246 :: utf8 >> },
    { "para", << 182 :: utf8 >> },
    { "part", << 8706 :: utf8 >> },
    { "permil", << 8240 :: utf8 >> },
    { "perp", << 8869 :: utf8 >> },
    { "Phi", << 934 :: utf8 >> },
    { "phi", << 966 :: utf8 >> },
    { "Pi", << 928 :: utf8 >> },
    { "pi", << 960 :: utf8 >> },
    { "piv", << 982 :: utf8 >> },
    { "plusmn", << 177 :: utf8 >> },
    { "pound", << 163 :: utf8 >> },
    { "Prime", << 8243 :: utf8 >> },
    { "prime", << 8242 :: utf8 >> },
    { "prod", << 8719 :: utf8 >> },
    { "prop", << 8733 :: utf8 >> },
    { "Psi", << 936 :: utf8 >> },
    { "psi", << 968 :: utf8 >> },
    { "quot", << 34 :: utf8 >> },
    { "radic", << 8730 :: utf8 >> },
    { "rang", << 9002 :: utf8 >> },
    { "raquo", << 187 :: utf8 >> },
    { "rArr", << 8658 :: utf8 >> },
    { "rarr", << 8594 :: utf8 >> },
    { "rceil", << 8969 :: utf8 >> },
    { "rdquo", << 8221 :: utf8 >> },
    { "real", << 8476 :: utf8 >> },
    { "reg", << 174 :: utf8 >> },
    { "rfloor", << 8971 :: utf8 >> },
    { "Rho", << 929 :: utf8 >> },
    { "rho", << 961 :: utf8 >> },
    { "rlm", << 8207 :: utf8 >> },
    { "rsaquo", << 8250 :: utf8 >> },
    { "rsquo", << 8217 :: utf8 >> },
    { "sbquo", << 8218 :: utf8 >> },
    { "Scaron", << 352 :: utf8 >> },
    { "scaron", << 353 :: utf8 >> },
    { "sdot", << 8901 :: utf8 >> },
    { "sect", << 167 :: utf8 >> },
    { "shy", << 173 :: utf8 >> },
    { "Sigma", << 931 :: utf8 >> },
    { "sigma", << 963 :: utf8 >> },
    { "sigmaf", << 962 :: utf8 >> },
    { "sim", << 8764 :: utf8 >> },
    { "spades", << 9824 :: utf8 >> },
    { "sub", << 8834 :: utf8 >> },
    { "sube", << 8838 :: utf8 >> },
    { "sum", << 8721 :: utf8 >> },
    { "sup", << 8835 :: utf8 >> },
    { "sup1", << 185 :: utf8 >> },
    { "sup2", << 178 :: utf8 >> },
    { "sup3", << 179 :: utf8 >> },
    { "supe", << 8839 :: utf8 >> },
    { "szlig", << 223 :: utf8 >> },
    { "Tau", << 932 :: utf8 >> },
    { "tau", << 964 :: utf8 >> },
    { "there4", << 8756 :: utf8 >> },
    { "Theta", << 920 :: utf8 >> },
    { "theta", << 952 :: utf8 >> },
    { "thetasym", << 977 :: utf8 >> },
    { "thinsp", << 8201 :: utf8 >> },
    { "THORN", << 222 :: utf8 >> },
    { "thorn", << 254 :: utf8 >> },
    { "tilde", << 732 :: utf8 >> },
    { "times", << 215 :: utf8 >> },
    { "trade", << 8482 :: utf8 >> },
    { "Uacute", << 218 :: utf8 >> },
    { "uacute", << 250 :: utf8 >> },
    { "uArr", << 8657 :: utf8 >> },
    { "uarr", << 8593 :: utf8 >> },
    { "Ucirc", << 219 :: utf8 >> },
    { "ucirc", << 251 :: utf8 >> },
    { "Ugrave", << 217 :: utf8 >> },
    { "ugrave", << 249 :: utf8 >> },
    { "uml", << 168 :: utf8 >> },
    { "upsih", << 978 :: utf8 >> },
    { "Upsilon", << 933 :: utf8 >> },
    { "upsilon", << 965 :: utf8 >> },
    { "Uuml", << 220 :: utf8 >> },
    { "uuml", << 252 :: utf8 >> },
    { "weierp", << 8472 :: utf8 >> },
    { "Xi", << 926 :: utf8 >> },
    { "xi", << 958 :: utf8 >> },
    { "Yacute", << 221 :: utf8 >> },
    { "yacute", << 253 :: utf8 >> },
    { "yen", << 165 :: utf8 >> },
    { "Yuml", << 376 :: utf8 >> },
    { "yuml", << 255 :: utf8 >> },
    { "Zeta", << 918 :: utf8 >> },
    { "zeta", << 950 :: utf8 >> },
    { "zwj", << 8205 :: utf8 >> },
    { "zwnj", << 8204 :: utf8 >> }
  ]

  def basic_entity do
    %r/[<>"&]/u
  end

  def extended_entity do
    %r/[^\x{20}-\x{7E}]|'/u
  end
end
