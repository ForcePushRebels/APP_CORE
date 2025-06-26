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
with Ada.Exceptions;
with Interfaces.C;
with Lumpiz_Binding;
with Mrpiz_Binding;
with Sensor_Manager;

procedure App_Core_Explo is
   use Ada.Text_IO;
   use Ada.Exceptions;
   use Interfaces.C;
   use Lumpiz_Binding;
   use Mrpiz_Binding;
   use Sensor_Manager;

   My_Sensor_Manager : Sensor_Manager_T;
   My_Sensor_Data : Sensor_Data_T;
   Luminosity : Float;
   Battery_Level : Integer;
   Battery_Voltage : Float;

begin
   Put_Line ("=== Démarrage du système d'exploration ===");

   begin
      -- Initialisation du robot MRPiZ avec le simulateur Intox
      Put_Line ("Initialisation du robot MRPiZ (Intox)...");
      Mrpiz_Init_Intox ("127.0.0.1", 12345);
      Put_Line ("Robot MRPiZ initialisé avec succès");

   exception
      when E : Mrpiz_Init_Error =>
         Put_Line ("Erreur lors de l'initialisation du robot MRPiZ : " & Exception_Message (E));
         return;
   end;

   begin
      -- Initialisation du gestionnaire de capteurs
      Put_Line ("Initialisation du gestionnaire de capteurs...");
      My_Sensor_Manager.Initialize;
      Put_Line ("Gestionnaire de capteurs initialisé");

      -- Démarrage du gestionnaire de capteurs
      Put_Line ("Démarrage du gestionnaire de capteurs...");
      My_Sensor_Manager.Start_Sensor_Manager;
      Put_Line ("Gestionnaire de capteurs démarré");

      -- Allumer la LED en vert pour indiquer que le système est prêt
      Put_Line ("Allumage de la LED en vert...");
      Mrpiz_Led_Rgb_Set (MRPIZ_LED_GREEN);
      Put_Line ("LED allumée en vert");

      -- Lecture des capteurs via le gestionnaire
      Put_Line ("=== Lecture des capteurs via le gestionnaire ===");
      My_Sensor_Manager.Get_Sensor_Data (My_Sensor_Data);
      Put_Line ("Données des capteurs récupérées");

      -- Affichage des données de luminosité
      My_Sensor_Manager.Get_Luminosity (Luminosity);
      Put_Line ("Luminosité :" & Float'Image (Luminosity));

      -- Affichage de la batterie
      My_Sensor_Manager.Get_Battery_Level (Battery_Level);
      My_Sensor_Manager.Get_Battery_Voltage (Battery_Voltage);
      Put_Line ("Niveau de batterie :" & Integer'Image (Battery_Level) & "%");
      Put_Line ("Tension de batterie :" & Float'Image (Battery_Voltage) & "V");

      -- Test de détection d'obstacle
      if Is_Obstacle_Detected (Threshold => 50) then
         Put_Line ("Obstacle détecté !");
      else
         Put_Line ("Aucun obstacle détecté");
      end if;

      -- Exemple de contrôle moteur (avancer doucement pendant un court instant)
      Put_Line ("=== Test des moteurs ===");
      Put_Line ("Avancement lent...");
      Mrpiz_Motor_Set (MRPIZ_MOTOR_BOTH, 20); -- 20% de vitesse
      delay 1.0; -- Attendre 1 seconde

      Put_Line ("Arrêt des moteurs...");
      Stop_All_Motors;

      -- Test de reset des encodeurs
      Put_Line ("Reset des encodeurs...");
      Mrpiz_Motor_Encoder_Reset (MRPIZ_MOTOR_BOTH);

      -- Éteindre la LED
      Put_Line ("Extinction de la LED...");
      Mrpiz_Led_Rgb_Set (MRPIZ_LED_OFF);

      -- Arrêt du gestionnaire de capteurs
      Put_Line ("Arrêt du gestionnaire de capteurs...");
      My_Sensor_Manager.Stop_Sensor_Manager;

   exception
      when E : Sensor_Manager_Error =>
         Put_Line ("Erreur du gestionnaire de capteurs : " & Exception_Message (E));
      when E : Mrpiz_Motor_Error =>
         Put_Line ("Erreur moteur : " & Exception_Message (E));
      when E : Mrpiz_Sensor_Error =>
         Put_Line ("Erreur capteur : " & Exception_Message (E));
      when E : Mrpiz_Led_Error =>
         Put_Line ("Erreur LED : " & Exception_Message (E));
      when E : others =>
         Put_Line ("Erreur inattendue : " & Exception_Message (E));
   end;

   -- Fermeture propre
   Put_Line ("Fermeture du système...");
   Mrpiz_Close;
   Put_Line ("=== Système d'exploration arrêté ===");

end App_Core_Explo;
