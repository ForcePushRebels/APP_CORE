with Ada.Text_IO;
with Interfaces.C;
with Lumpiz_Binding;
with Mrpiz_Binding;

procedure App_Core_Explo is
   use Ada.Text_IO;
   use Interfaces.C;
   use Lumpiz_Binding;
   use Mrpiz_Binding;

   Result : int;
   Luminosity : int;
   Proximity : int;
   Battery_Level : int;
   Battery_Voltage : C_float;

begin
   Put_Line ("=== Démarrage du système d'exploration ===");

   -- Initialisation du robot MRPiZ
   Put_Line ("Initialisation du robot MRPiZ...");
   Result := Mrpiz_Init;
   if Result /= 0 then
      Put_Line ("Erreur lors de l'initialisation du robot MRPiZ");
      return;
   end if;
   Put_Line ("Robot MRPiZ initialisé avec succès");

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
   Result := Mrpiz_Led_Rgb_Set (MRPIZ_LED_GREEN);
   if Result /= 0 then
      Put_Line ("Erreur lors de l'allumage de la LED");
   end if;

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
   if Proximity >= 0 then
      Put_Line ("Proximité centrale :" & int'Image (Proximity));
   else
      Put_Line ("Erreur de lecture du capteur de proximité");
   end if;

   -- Lecture du niveau de batterie
   Battery_Level := Mrpiz_Battery_Level;
   Put_Line ("Niveau de batterie :" & int'Image (Battery_Level) & "%");

   Battery_Voltage := Mrpiz_Battery_Voltage;
   Put_Line ("Tension de batterie :" & C_float'Image (Battery_Voltage) & "V");

   -- Exemple de contrôle moteur (avancer doucement pendant un court instant)
   Put_Line ("=== Test des moteurs ===");
   Put_Line ("Avancement lent...");
   Result := Mrpiz_Motor_Set (MRPIZ_MOTOR_BOTH, 20); -- 20% de vitesse
   delay 1.0; -- Attendre 1 seconde

   Put_Line ("Arrêt des moteurs...");
   Result := Mrpiz_Motor_Set (MRPIZ_MOTOR_BOTH, 0); -- Arrêt

   -- Éteindre la LED
   Put_Line ("Extinction de la LED...");
   Result := Mrpiz_Led_Rgb_Set (MRPIZ_LED_OFF);

   -- Éteindre le capteur de luminosité
   Put_Line ("Extinction du capteur de luminosité...");
   Result := Lumpiz_Luminosity_Light_Set (LUMPIZ_LUMINOSITY_OFF);

   -- Fermeture propre
   Put_Line ("Fermeture du système...");
   Mrpiz_Close;
   Put_Line ("=== Système d'exploration arrêté ===");

end App_Core_Explo;
