-------------------------------------------------
-- Corps du binding Ada pour la bibliothèque mrpiz --
-- Robot MRPiZ                                      --
-- Implémentation avec gestion d'erreur             --
-------------------------------------------------

package body Mrpiz_Binding is

   -- Variable pour suivre l'état d'initialisation
   Is_Initialized : Boolean := False;

   -- Vérification que le système est initialisé
   procedure Check_Initialized is
   begin
      if not Is_Initialized then
         raise Mrpiz_Init_Error with "Robot non initialisé. Appelez Mrpiz_Init d'abord.";
      end if;
   end Check_Initialized;

   ---------------------------------
   -- Fonctions de haut niveau    --
   ---------------------------------

   -- Initialisation avec gestion d'erreur
   procedure Mrpiz_Init is
      Result : int;
   begin
      Result := Mrpiz_Init_Raw;
      if Result /= 0 then
         raise Mrpiz_Init_Error with "Échec de l'initialisation du robot MRPiZ";
      end if;
      Is_Initialized := True;
   end Mrpiz_Init;

   -- Fermeture propre
   procedure Mrpiz_Close is
   begin
      if Is_Initialized then
         Mrpiz_Close_Raw;
         Is_Initialized := False;
      end if;
   end Mrpiz_Close;

   -- Contrôle moteur avec vérification
   procedure Mrpiz_Motor_Set (Id : Mrpiz_Motor_Id; Cmd : Motor_Command_Percent) is
      Result : int;
   begin
      Check_Initialized;
      Result := Mrpiz_Motor_Set_Raw (Id, int (Cmd));
      if Result /= 0 then
         raise Mrpiz_Motor_Error with "Erreur lors du contrôle du moteur";
      end if;
   end Mrpiz_Motor_Set;

   -- Lecture encodeur avec vérification
   function Mrpiz_Motor_Encoder_Get (Id : Mrpiz_Motor_Id) return int is
      Result : int;
   begin
      Check_Initialized;
      -- Note: Pour cette fonction, il faut vérifier errno en cas d'erreur
      -- mais pour simplifier, on fait confiance à la valeur retournée
      Result := Mrpiz_Motor_Encoder_Get_Raw (Id);
      return Result;
   end Mrpiz_Motor_Encoder_Get;

   -- Reset encodeur avec vérification
   procedure Mrpiz_Motor_Encoder_Reset (Id : Mrpiz_Motor_Id) is
      Result : int;
   begin
      Check_Initialized;
      Result := Mrpiz_Motor_Encoder_Reset_Raw (Id);
      if Result /= 0 then
         raise Mrpiz_Motor_Error with "Erreur lors du reset de l'encodeur";
      end if;
   end Mrpiz_Motor_Encoder_Reset;

   -- Lecture capteur de proximité avec vérification
   function Mrpiz_Proxy_Sensor_Get (Id : Mrpiz_Proxy_Sensor_Id) return Sensor_Value is
      Result : int;
   begin
      Check_Initialized;
      Result := Mrpiz_Proxy_Sensor_Get_Raw (Id);
      if Result < 0 then
         raise Mrpiz_Sensor_Error with "Erreur lors de la lecture du capteur de proximité";
      end if;
      return Sensor_Value (Result);
   end Mrpiz_Proxy_Sensor_Get;

   -- Contrôle LED avec vérification
   procedure Mrpiz_Led_Rgb_Set (Color : Mrpiz_Led_Rgb_Color_T) is
      Result : int;
   begin
      Check_Initialized;
      Result := Mrpiz_Led_Rgb_Set_Raw (Color);
      if Result /= 0 then
         raise Mrpiz_Led_Error with "Erreur lors du contrôle de la LED";
      end if;
   end Mrpiz_Led_Rgb_Set;

   -- Tension batterie
   function Mrpiz_Battery_Voltage return Float is
   begin
      Check_Initialized;
      return Float (Mrpiz_Battery_Voltage_Raw);
   end Mrpiz_Battery_Voltage;

   -- Niveau batterie avec vérification
   function Mrpiz_Battery_Level return Battery_Level_Percent is
      Result : int;
   begin
      Check_Initialized;
      Result := Mrpiz_Battery_Level_Raw;
      -- Assume que la fonction retourne une valeur valide
      return Battery_Level_Percent (Result);
   end Mrpiz_Battery_Level;

   ---------------------------------
   -- Fonctions utilitaires       --
   ---------------------------------

   -- Arrêt de tous les moteurs
   procedure Stop_All_Motors is
   begin
      Mrpiz_Motor_Set (MRPIZ_MOTOR_BOTH, 0);
   end Stop_All_Motors;

   -- Détection d'obstacle basée sur tous les capteurs avant
   function Is_Obstacle_Detected (Threshold : Sensor_Value := 100) return Boolean is
      Sensors : constant array (1 .. 5) of Mrpiz_Proxy_Sensor_Id :=
        (MRPIZ_PROXY_SENSOR_FRONT_LEFT,
         MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT,
         MRPIZ_PROXY_SENSOR_FRONT_CENTER,
         MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT,
         MRPIZ_PROXY_SENSOR_FRONT_RIGHT);
   begin
      for Sensor of Sensors loop
         if Mrpiz_Proxy_Sensor_Get (Sensor) > Threshold then
            return True;
         end if;
      end loop;
      return False;
   end Is_Obstacle_Detected;

end Mrpiz_Binding; 