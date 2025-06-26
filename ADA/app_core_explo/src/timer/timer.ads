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

with Ada.Real_Time;

package Timer is

   -- Type pour la durée en millisecondes
   type Milliseconds is new Natural;

   -- Type de callback - procédure sans paramètre
   type Callback_Access is access procedure;

   -- Type pour identifier un timer
   type Timer_Id_T is private;

   -- Type pour indiquer si un timer est périodique ou non
   type Timer_Mode is (One_Shot, Periodic);

   -- Exceptions
   Timer_Already_Running : exception;
   Timer_Not_Running : exception;
   Invalid_Timer_Id : exception;

   -- Création d'un timer
   procedure Create_Timer
     (Timer_Id  : out Timer_Id_T;
      Interval  : Milliseconds;
      Callback  : Callback_Access;
      Mode      : Timer_Mode := One_Shot);

   -- Démarrer un timer
   procedure Start_Timer (Timer_Id : Timer_Id_T);

   -- Arrêter un timer
   procedure Stop_Timer (Timer_Id : Timer_Id_T);

   -- Vérifier si un timer est actif
   function Is_Running (Timer_Id : Timer_Id_T) return Boolean;

   -- Modifier l'intervalle d'un timer (doit être arrêté)
   procedure Set_Interval (Timer_Id : Timer_Id_T; Interval : Milliseconds);

   -- Obtenir l'intervalle d'un timer
   function Get_Interval (Timer_Id : Timer_Id_T) return Milliseconds;

   -- Détruire un timer
   procedure Destroy_Timer (Timer_Id : Timer_Id_T);

private

   -- Task pour gérer un timer individuel
   task type Timer_Task_T is
      entry Start (Interval : Ada.Real_Time.Time_Span; 
                   Callback : Callback_Access; 
                   Mode : Timer_Mode);
      entry Stop;
      entry Terminate_Task;
   end Timer_Task_T;

   type Timer_Task_Access is access Timer_Task_T;

   -- Structure interne du timer
   type Timer_Record_T is record
      Task_Access : Timer_Task_Access;
      Interval    : Milliseconds;
      Callback    : Callback_Access;
      Mode        : Timer_Mode;
      Is_Created  : Boolean := False;
      Is_Running  : Boolean := False;
   end record;

   type Timer_Id_T is access Timer_Record_T;

end Timer;