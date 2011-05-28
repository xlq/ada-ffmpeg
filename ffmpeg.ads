with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;
with System;

package FFmpeg is
   package C renames Interfaces.C;
   use type C.int;

   subtype Void_Pointer is System.Address;
   Null_Pointer: constant Void_Pointer := System.Null_Address;

   type unsigned_array is array (Positive range <>) of aliased C.unsigned;
   pragma Convention (C, unsigned_array);

   type uint8_t is mod 2**8;
   for uint8_t'Size use 8;
   pragma Convention (C, uint8_t);
   type uint8_array is array (Natural range <>)
      of aliased uint8_t;
   pragma Convention (C, uint8_array);
   package uint8_pointers is new Interfaces.C.Pointers
     (Natural, uint8_t, uint8_array, 0);
   subtype uint8_access is uint8_pointers.Pointer;
   --type uint8_Access is access uint8_t;
   --pragma Convention (C, uint8_Access);
   type uint8_access_array is array (Natural range <>)
      of aliased uint8_access;
   pragma Convention (C, uint8_access_array);
   type uint8_access_access is access uint8_access;
   pragma Convention (C, uint8_access_access);

   type int8_t is range -2**7+1..2**7-1;
   for int8_t'Size use 8;
   pragma Convention (C, int8_t);
   type int8_access is access int8_t;
   pragma Convention (C, int8_access);
   type int8_access_array is array (Natural range <>)
      of aliased int8_access;
   pragma Convention (C, int8_access_array);

   type int16_t is range -2**15+1..2**15-1;
   for int16_t'Size use 16;
   pragma Convention (C, int16_t);
   type int16_access is access int16_t;
   pragma Convention (C, int16_access);

   type uint16_t is mod 2**16;
   for uint16_t'Size use 16;
   pragma Convention (C, uint16_t);
   type uint16_access is access uint16_t;
   pragma Convention (C, uint16_access);

   type uint32_t is mod 2**32;
   for uint32_t'Size use 32;
   pragma Convention (C, uint32_t);
   type uint32_access is access uint32_t;
   pragma Convention (C, uint32_access);

   type uint64_t is mod 2**64;
   for uint64_t'Size use 64;
   pragma Convention (C, uint64_t);
   subtype int64_t is uint64_t; -- XXX: Wrong!

   type uint64_array is array (Natural range <>) of aliased uint64_t;
   pragma Convention (C, uint64_array);

   type short_access is access all C.short;
   pragma Convention (C, short_access);

   type int_access is access all C.int;
   pragma Convention (C, int_access);
   type int_array is array (Natural range <>) of aliased C.int;
   pragma Convention (C, int_array);

   type double_access is access all C.double;
   pragma Convention (C, double_access);

   type Error_Code is new C.int;
   OK: constant Error_Code := 0;
end FFmpeg;
