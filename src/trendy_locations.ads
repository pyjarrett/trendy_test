with Interfaces.C.Strings;

-- Source code reporting using GCC built-ins to avoid dependencies on GNAT
-- libraries.
package Trendy_Locations is
    subtype Char_Ptr is Interfaces.C.Strings.chars_ptr;
    function File_Line return Natural;
    function File_Name return Char_Ptr;
    function Subprogram_Name return Char_Ptr;
    function Image (Str : Char_Ptr) return String renames Interfaces.C.Strings.Value;
    pragma Import (Intrinsic, File_Line, "__builtin_LINE");
    pragma Import (Intrinsic, File_Name, "__builtin_FILE");
    pragma Import (Intrinsic, Subprogram_Name, "__builtin_FUNCTION");

    -- Prevent from having to lug around files and lines separately by
    -- simply making them part of the same group.
    type Source_Location is record
        File : Char_Ptr;
        Line : Natural;
    end record;

    -- Call with no parameters to make a file/line location at the current
    -- in the file.
    function Make_Source_Location (File : Char_Ptr := File_Name;
                                    Line : Natural := File_Line) return Source_Location;

    function Image (Loc : Source_Location) return String;
end Trendy_Locations;
