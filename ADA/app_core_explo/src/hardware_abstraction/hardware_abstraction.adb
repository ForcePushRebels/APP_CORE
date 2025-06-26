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

with Ada.Finalization;
with Interfaces.C;
with Mrpiz_Binding;
with Lumpiz_Binding;

package body Hardware_Abstraction is

   use Ada.Finalization;
   use Interfaces.C;
   use Mrpiz_Binding;
   use Lumpiz_Binding;

   overriding
   procedure Initialize (Self : in out Hardware_Abstraction_T) is
   begin
      null;
   end Initialize;

   function Get_Luminosity (Self : in out Hardware_Abstraction_T) return Float is
   pragma Unreferenced (Self);
   begin
      declare
         l_iReturn : int := 0;
      begin
         l_iReturn := Lumpiz_Luminosity_Get;
         if l_iReturn >= 0 then
            return Float (l_iReturn);
         else
            return 0.0;
         end if;
      end;
   end Get_Luminosity;

   function Get_Proximity (Self : in out Hardware_Abstraction_T) return Float is
   pragma Unreferenced (Self);
   begin
      declare
         l_iReturn : int := 0;
      begin
         l_iReturn := Mrpiz_Proxy_Sensor_Get (MRPIZ_PROXY_SENSOR_FRONT_CENTER);
         return Float (l_iReturn);
      end;
   end Get_Proximity;

   function Get_Battery_Level (Self : in out Hardware_Abstraction_T) return Float is
   pragma Unreferenced (Self);
   begin
      declare
         l_iReturn : Battery_Level_Percent := 0;
      begin
         l_iReturn := Mrpiz_Battery_Level;
         return Float (l_iReturn);
      end;
   end Get_Battery_Level;

   function Get_Battery_Voltage (Self : in out Hardware_Abstraction_T) return Float is
   pragma Unreferenced (Self);
   begin
      return Mrpiz_Battery_Voltage;
   end Get_Battery_Voltage;

end Hardware_Abstraction;