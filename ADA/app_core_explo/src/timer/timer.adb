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
with Ada.Unchecked_Deallocation;

package body Timer is

   use Ada.Real_Time;

   -- Conversion millisecondes vers Time_Span
   function Ms_To_Time_Span (Ms : Milliseconds) return Time_Span is
   begin
      return Milliseconds (Ms) * Milliseconds (1_000_000);
   end Ms_To_Time_Span;

   -- Implémentation de la tâche timer
   task body Timer_Task_T is
      Timer_Interval : Time_Span;
      Timer_Callback : Callback_Access;
      Timer_Mode     : Timer_Mode;
      Running        : Boolean := False;
      Next_Time      : Time;
   begin
      loop
         select
            -- Démarrer le timer
            accept Start (Interval : Time_Span; 
                         Callback : Callback_Access; 
                         Mode : Timer_Mode) do
               Timer_Interval := Interval;
               Timer_Callback := Callback;
               Timer_Mode := Mode;
               Running := True;
               Next_Time := Clock + Timer_Interval;
            end Start;

            -- Boucle principale du timer
            while Running loop
               select
                  -- Arrêter le timer
                  accept Stop do
                     Running := False;
                  end Stop;
               or
                  -- Terminer la tâche
                  accept Terminate_Task do
                     Running := False;
                     exit;
                  end Terminate_Task;
               or
                  -- Attendre l'expiration du timer
                  delay until Next_Time;

                  -- Exécuter le callback si la tâche n'est pas arrêtée
                  if Running and then Timer_Callback /= null then
                     begin
                        Timer_Callback.all;
                     exception
                        when others =>
                           -- Ignorer les exceptions dans le callback
                           null;
                     end;
                  end if;

                  -- Gérer le mode périodique
                  if Running then
                     case Timer_Mode is
                        when One_Shot =>
                           Running := False;
                        when Periodic =>
                           Next_Time := Next_Time + Timer_Interval;
                     end case;
                  end if;
               end select;
            end loop;
         or
            -- Terminer la tâche
            accept Terminate_Task do
               Running := False;
               exit;
            end Terminate_Task;
         end select;
      end loop;
   end Timer_Task_T;

   -- Création d'un timer
   procedure Create_Timer
     (Timer_Id  : out Timer_Id_T;
      Interval  : Milliseconds;
      Callback  : Callback_Access;
      Mode      : Timer_Mode := One_Shot) is
   begin
      Timer_Id := new Timer_Record_T;
      Timer_Id.Task_Access := new Timer_Task_T;
      Timer_Id.Interval := Interval;
      Timer_Id.Callback := Callback;
      Timer_Id.Mode := Mode;
      Timer_Id.Is_Created := True;
      Timer_Id.Is_Running := False;
   end Create_Timer;

   -- Démarrer un timer
   procedure Start_Timer (Timer_Id : Timer_Id_T) is
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      if Timer_Id.Is_Running then
         raise Timer_Already_Running;
      end if;

      Timer_Id.Task_Access.Start 
        (Interval => Ms_To_Time_Span (Timer_Id.Interval),
         Callback => Timer_Id.Callback,
         Mode     => Timer_Id.Mode);
      Timer_Id.Is_Running := True;
   end Start_Timer;

   -- Arrêter un timer
   procedure Stop_Timer (Timer_Id : Timer_Id_T) is
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      if not Timer_Id.Is_Running then
         raise Timer_Not_Running;
      end if;

      Timer_Id.Task_Access.Stop;
      Timer_Id.Is_Running := False;
   end Stop_Timer;

   -- Vérifier si un timer est actif
   function Is_Running (Timer_Id : Timer_Id_T) return Boolean is
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      return Timer_Id.Is_Running;
   end Is_Running;

   -- Modifier l'intervalle d'un timer
   procedure Set_Interval (Timer_Id : Timer_Id_T; Interval : Milliseconds) is
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      if Timer_Id.Is_Running then
         raise Timer_Already_Running;
      end if;

      Timer_Id.Interval := Interval;
   end Set_Interval;

   -- Obtenir l'intervalle d'un timer
   function Get_Interval (Timer_Id : Timer_Id_T) return Milliseconds is
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      return Timer_Id.Interval;
   end Get_Interval;

   -- Détruire un timer
   procedure Destroy_Timer (Timer_Id : Timer_Id_T) is
      procedure Free_Timer_Record is new Ada.Unchecked_Deallocation 
        (Timer_Record_T, Timer_Id_T);
      procedure Free_Timer_Task is new Ada.Unchecked_Deallocation 
        (Timer_Task_T, Timer_Task_Access);
   begin
      if Timer_Id = null or else not Timer_Id.Is_Created then
         raise Invalid_Timer_Id;
      end if;

      -- Arrêter la tâche si elle est en cours
      if Timer_Id.Is_Running then
         Timer_Id.Task_Access.Stop;
      end if;

      -- Terminer la tâche
      Timer_Id.Task_Access.Terminate_Task;

      -- Libérer la mémoire
      Free_Timer_Task (Timer_Id.Task_Access);
      Timer_Id.Is_Created := False;
      Free_Timer_Record (Timer_Id);
   end Destroy_Timer;

end Timer;