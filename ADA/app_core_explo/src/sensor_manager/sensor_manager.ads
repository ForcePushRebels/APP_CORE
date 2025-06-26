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
with Ada.Real_Time;
with Timer;
with Hardware_Abstraction;
with Ada.Exceptions;

package Sensor_Manager is

   Sensor_Manager_Error : exception;

   type Sensor_Data_T is private;
   
   type Sensor_Manager_T is new Ada.Finalization.Limited_Controlled with private;

   overriding
   procedure Initialize (Self : in out Sensor_Manager_T);

   procedure Start_Sensor_Manager (Self : in out Sensor_Manager_T);

   procedure Stop_Sensor_Manager (Self : in out Sensor_Manager_T);

   procedure Get_Sensor_Data (Self : in out Sensor_Manager_T;
                              Sensor_Data : out Sensor_Data_T);

   procedure Get_Luminosity (Self : in out Sensor_Manager_T;
                             Luminosity : out Float);

   procedure Get_Battery_Level (Self : in out Sensor_Manager_T;
                                Battery_Level : out Integer);

   procedure Get_Battery_Voltage (Self : in out Sensor_Manager_T;
                                  Battery_Voltage : out Float);

private
   type Proximity_Array_T is array (1 .. 3) of Float;
   subtype Battery_Level_T is Integer range 0 .. 100;
   subtype Battery_Voltage_T is Float range 0.0 .. 100.0;

   type Sensor_Data_T is record
      Luminosity      : Float;
      Proximity       : Proximity_Array_T;
      Battery_Level   : Battery_Level_T;
      Battery_Voltage : Battery_Voltage_T;
   end record;

   type Sensor_Manager_T is new Ada.Finalization.Limited_Controlled with record
      Luminosity_Timer : Timer.Timer_Id_T;
      Proximity_Timer  : Timer.Timer_Id_T;
      Battery_Timer    : Timer.Timer_Id_T;
      Sensor_Data      : Sensor_Data_T;
      Hardware_Abs     : Hardware_Abstraction.Hardware_Abstraction_T;
   end record;

   task Sensor_Manager_Task_T is
      entry Start_Sensor_Manager;
   end Sensor_Manager_Task_T;
      
end Sensor_Manager;
