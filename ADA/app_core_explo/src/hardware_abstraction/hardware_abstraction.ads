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

package Hardware_Abstraction is

   type Hardware_Abstraction_T is new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Initialize (Self : in out Hardware_Abstraction_T);

   function Get_Luminosity (Self : in out Hardware_Abstraction_T) return Float;

   function Get_Proximity (Self : in out Hardware_Abstraction_T) return Float;

   function Get_Battery_Level (Self : in out Hardware_Abstraction_T) return Float;

   function Get_Battery_Voltage (Self : in out Hardware_Abstraction_T) return Float;

private

   type Hardware_Abstraction_T is new Ada.Finalization.Limited_Controlled with null record;
   
end Hardware_Abstraction;