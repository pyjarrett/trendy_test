with Ada.Strings.Fixed;

package body Trendy_Locations is
    function Make_Source_Location (File : Char_Ptr := File_Name;
                                    Line : Natural := File_Line) return Source_Location is
    begin
        return (File, Line);
    end Make_Source_Location;

    function Image (Loc : Source_Location) return String is
        use Ada.Strings;
        use Ada.Strings.Fixed;
    begin
        -- The trimming step here removes the extra space printed to the
        -- left and hence reports filename:line which editors can use to
        -- jump directly to errors.
        return Image (Loc.File) & ':' & Trim (Loc.Line'Image, Left);
    end Image;
end Trendy_Locations;
