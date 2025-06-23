with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;
with Lumpiz_Binding;
with Mrpiz_Binding;

procedure App_Core_Explo is
   use Ada.Text_IO;
   use Ada.Exceptions;
   use Interfaces.C;
   use Lumpiz_Binding;
   use Mrpiz_Binding;

   Result : int;
   Luminosity : int;
   Proximity : Sensor_Value;
   Battery_Level : Battery_Level_Percent;
   Battery_Voltage : Float;

begin
   Put_Line ("=== Démarrage du système d'exploration ===");

   begin
      -- Initialisation du robot MRPiZ
      Put_Line ("Initialisation du robot MRPiZ...");
      Mrpiz_Init;
      Put_Line ("Robot MRPiZ initialisé avec succès");

   exception
      when E : Mrpiz_Init_Error =>
         Put_Line ("Erreur lors de l'initialisation du robot MRPiZ : " & Exception_Message (E));
         return;
   end;

   begin
      -- Initialisation du capteur de luminosité
      Put_Line ("Initialisation du capteur de luminosité...");
      Result := Lumpiz_Luminosity_Init;
      if Result /= 0 then
         Put_Line ("Erreur lors de l'initialisation du capteur de luminosité");
         Mrpiz_Close;
         return;
      end if;
      Put_Line ("Capteur de luminosité initialisé avec succès");

      -- Allumer le capteur de luminosité
      Put_Line ("Allumage du capteur de luminosité...");
      Result := Lumpiz_Luminosity_Light_Set (LUMPIZ_LUMINOSITY_ON);
      if Result /= 0 then
         Put_Line ("Erreur lors de l'allumage du capteur");
      end if;

      -- Allumer la LED en vert pour indiquer que le système est prêt
      Put_Line ("Allumage de la LED en vert...");
      Mrpiz_Led_Rgb_Set (MRPIZ_LED_GREEN);
      Put_Line ("LED allumée en vert");

      -- Exemple de lecture des capteurs
      Put_Line ("=== Lecture des capteurs ===");

      -- Lecture de la luminosité
      Luminosity := Lumpiz_Luminosity_Get;
      if Luminosity >= 0 then
         Put_Line ("Luminosité actuelle :" & int'Image (Luminosity));
      else
         Put_Line ("Erreur de lecture du capteur de luminosité");
      end if;

      -- Lecture du capteur de proximité central
      Proximity := Mrpiz_Proxy_Sensor_Get (MRPIZ_PROXY_SENSOR_FRONT_CENTER);
      Put_Line ("Proximité centrale :" & Sensor_Value'Image (Proximity));

      -- Lecture du niveau de batterie
      Battery_Level := Mrpiz_Battery_Level;
      Put_Line ("Niveau de batterie :" & Battery_Level_Percent'Image (Battery_Level) & "%");

      Battery_Voltage := Mrpiz_Battery_Voltage;
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

      -- Éteindre le capteur de luminosité
      Put_Line ("Extinction du capteur de luminosité...");
      Result := Lumpiz_Luminosity_Light_Set (LUMPIZ_LUMINOSITY_OFF);

   exception
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
