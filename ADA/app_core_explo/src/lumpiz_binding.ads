----------------------------------------------------------------------
--  Copyright (C) 2025 
-- 
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
-- 
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
-- 
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------

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