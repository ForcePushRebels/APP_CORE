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

with Ada.Text_IO;
with Ada.Real_Time;
with Ada.Exceptions;
with Interfaces.C;
with Timer;
with Hardware_Abstraction;

package body Sensor_Manager is

   use Ada.Text_IO;
   use Ada.Real_Time;
   use Ada.Exceptions;
   use Interfaces.C;
   use Timer;
   use Hardware_Abstraction;


   overriding
   procedure Initialize (Self : in out Sensor_Manager_T) is
   begin
      -- Initialisation des timers
      Create_Timer (Self.Luminosity_Timer, 1000, null, Periodic);
      Create_Timer (Self.Proximity_Timer, 1000, null, Periodic);
      Create_Timer (Self.Battery_Timer, 1000, null, Periodic);

      -- Initialisation des données de capteurs
      Self.Sensor_Data.Luminosity := 0.0;
      Self.Sensor_Data.Proximity := (others => 0.0);
      Self.Sensor_Data.Battery_Level := 0;
      Self.Sensor_Data.Battery_Voltage := 0.0;
   end Initialize;
   

   procedure Start_Sensor_Manager (Self : in out Sensor_Manager_T) is
   begin
      Start_Timer (Self.Luminosity_Timer);
      Start_Timer (Self.Proximity_Timer);
      Start_Timer (Self.Battery_Timer);
   end Start_Sensor_Manager;
   
   procedure Stop_Sensor_Manager (Self : in out Sensor_Manager_T) is
   begin
      Stop_Timer (Self.Luminosity_Timer);
      Stop_Timer (Self.Proximity_Timer);
      Stop_Timer (Self.Battery_Timer);
   end Stop_Sensor_Manager;
   
   procedure Get_Sensor_Data (Self : in out Sensor_Manager_T;
                              Sensor_Data : out Sensor_Data_T) is
   begin
      -- Lecture de la luminosité
      Self.Sensor_Data.Luminosity := Get_Luminosity (Self.Hardware_Abs);

      -- Lecture des capteurs de proximité (utiliser le même pour tous)
      declare
         Proximity_Value : Float := Get_Proximity (Self.Hardware_Abs);
      begin
         Self.Sensor_Data.Proximity (1) := Proximity_Value;
         Self.Sensor_Data.Proximity (2) := Proximity_Value;
         Self.Sensor_Data.Proximity (3) := Proximity_Value;
      end;

      -- Lecture de la batterie
      Self.Sensor_Data.Battery_Level := Integer (Get_Battery_Level (Self.Hardware_Abs));
      Self.Sensor_Data.Battery_Voltage := Get_Battery_Voltage (Self.Hardware_Abs);

      -- Retourner les données
      Sensor_Data := Self.Sensor_Data;
   end Get_Sensor_Data;

   procedure Get_Luminosity (Self : in out Sensor_Manager_T;
                             Luminosity : out Float) is
   begin
      Luminosity := Self.Sensor_Data.Luminosity;
   end Get_Luminosity;

   procedure Get_Battery_Level (Self : in out Sensor_Manager_T;
                                Battery_Level : out Integer) is
   begin
      Battery_Level := Self.Sensor_Data.Battery_Level;
   end Get_Battery_Level;

   procedure Get_Battery_Voltage (Self : in out Sensor_Manager_T;
                                  Battery_Voltage : out Float) is
   begin
      Battery_Voltage := Self.Sensor_Data.Battery_Voltage;
   end Get_Battery_Voltage;

   -- Implémentation vide de la tâche pour satisfaire le compilateur
   task body Sensor_Manager_Task_T is
   begin
      accept Start_Sensor_Manager;
      -- Boucle simple pour maintenir la tâche active
      loop
         delay 1.0;
      end loop;
   end Sensor_Manager_Task_T;
   
end Sensor_Manager;