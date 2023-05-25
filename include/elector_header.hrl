-define(NAME(BaseName, ProcessSuffix),
        list_to_atom(atom_to_list(BaseName)
                     ++ "_"
                     ++ ProcessSuffix)).% TODO: use it for creating the singleton overviwer
