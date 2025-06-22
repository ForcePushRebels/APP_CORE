-------------------------------------------------
-- Binding Ada pour la bibliothèque lumpiz    --
-- Capteur de luminosité pour PiZ              --
-------------------------------------------------

with Interfaces.C;

package Lumpiz_Binding is

   use Interfaces.C;

   -- Types exportés de la bibliothèque C
   type Lumpiz_Luminosity_State_T is (
      LUMPIZ_LUMINOSITY_OFF,
      LUMPIZ_LUMINOSITY_ON
   );
   for Lumpiz_Luminosity_State_T use (
      LUMPIZ_LUMINOSITY_OFF => 0,
      LUMPIZ_LUMINOSITY_ON  => 1
   );
   pragma Convention (C, Lumpiz_Luminosity_State_T);

   -- Fonctions importées de la bibliothèque C
   function Lumpiz_Luminosity_Init return int;
   pragma Import (C, Lumpiz_Luminosity_Init, "lumpiz_luminosity_init");

   function Lumpiz_Luminosity_Light_Set (State : Lumpiz_Luminosity_State_T) return int;
   pragma Import (C, Lumpiz_Luminosity_Light_Set, "lumpiz_luminosity_light_set");

   function Lumpiz_Luminosity_Light_Get return Lumpiz_Luminosity_State_T;
   pragma Import (C, Lumpiz_Luminosity_Light_Get, "lumpiz_luminosity_light_get");

   function Lumpiz_Luminosity_Get return int;
   pragma Import (C, Lumpiz_Luminosity_Get, "lumpiz_luminosity_get");

end Lumpiz_Binding; 