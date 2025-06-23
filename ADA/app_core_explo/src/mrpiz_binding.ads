-------------------------------------------------
-- Binding Ada pour la bibliothèque mrpiz     --
-- Robot MRPiZ                                 --
-- Version améliorée avec gestion d'erreur    --
-------------------------------------------------

with Interfaces.C;

package Mrpiz_Binding is

   use Interfaces.C;

   -- Constantes
   MRPIZ_ENCODE_PER_TURN : constant := 390;

   -- Types d'exception pour la gestion d'erreur
   Mrpiz_Init_Error : exception;
   Mrpiz_Motor_Error : exception;
   Mrpiz_Sensor_Error : exception;
   Mrpiz_Led_Error : exception;

   -- Types exportés de la bibliothèque C
   type Mrpiz_Motor_Id is (
      MRPIZ_MOTOR_LEFT,
      MRPIZ_MOTOR_RIGHT,
      MRPIZ_MOTOR_BOTH
   );
   for Mrpiz_Motor_Id use (
      MRPIZ_MOTOR_LEFT  => 0,
      MRPIZ_MOTOR_RIGHT => 1,
      MRPIZ_MOTOR_BOTH  => 2
   );
   pragma Convention (C, Mrpiz_Motor_Id);

   type Mrpiz_Proxy_Sensor_Id is (
      MRPIZ_PROXY_SENSOR_FRONT_LEFT,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT,
      MRPIZ_PROXY_SENSOR_FRONT_RIGHT
   );
   for Mrpiz_Proxy_Sensor_Id use (
      MRPIZ_PROXY_SENSOR_FRONT_LEFT         => 1,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER_LEFT  => 2,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER       => 3,
      MRPIZ_PROXY_SENSOR_FRONT_CENTER_RIGHT => 4,
      MRPIZ_PROXY_SENSOR_FRONT_RIGHT        => 5
   );
   pragma Convention (C, Mrpiz_Proxy_Sensor_Id);

   type Mrpiz_Led_Rgb_Color_T is (
      MRPIZ_LED_OFF,
      MRPIZ_LED_RED,
      MRPIZ_LED_GREEN,
      MRPIZ_LED_BLUE
   );
   for Mrpiz_Led_Rgb_Color_T use (
      MRPIZ_LED_OFF   => 0,
      MRPIZ_LED_RED   => 1,
      MRPIZ_LED_GREEN => 2,
      MRPIZ_LED_BLUE  => 3
   );
   pragma Convention (C, Mrpiz_Led_Rgb_Color_T);

   -- Sous-types pour plus de sécurité
   subtype Motor_Command_Percent is int range -100 .. 100;
   subtype Sensor_Value is int range 0 .. 255;
   subtype Battery_Level_Percent is int range 0 .. 100;

   -- Fonctions d'initialisation et fermeture (niveau bas)
   function Mrpiz_Init_Raw return int;
   pragma Import (C, Mrpiz_Init_Raw, "mrpiz_init");

   procedure Mrpiz_Close_Raw;
   pragma Import (C, Mrpiz_Close_Raw, "mrpiz_close");

   -- Fonctions des moteurs (niveau bas)
   function Mrpiz_Motor_Set_Raw (Id : Mrpiz_Motor_Id; Cmd : int) return int;
   pragma Import (C, Mrpiz_Motor_Set_Raw, "mrpiz_motor_set");

   function Mrpiz_Motor_Encoder_Get_Raw (Id : Mrpiz_Motor_Id) return int;
   pragma Import (C, Mrpiz_Motor_Encoder_Get_Raw, "mrpiz_motor_encoder_get");

   function Mrpiz_Motor_Encoder_Reset_Raw (Id : Mrpiz_Motor_Id) return int;
   pragma Import (C, Mrpiz_Motor_Encoder_Reset_Raw, "mrpiz_motor_encoder_reset");

   -- Fonctions des capteurs de proximité (niveau bas)
   function Mrpiz_Proxy_Sensor_Get_Raw (Id : Mrpiz_Proxy_Sensor_Id) return int;
   pragma Import (C, Mrpiz_Proxy_Sensor_Get_Raw, "mrpiz_proxy_sensor_get");

   -- Fonctions LED RGB (niveau bas)
   function Mrpiz_Led_Rgb_Set_Raw (Color : Mrpiz_Led_Rgb_Color_T) return int;
   pragma Import (C, Mrpiz_Led_Rgb_Set_Raw, "mrpiz_led_rgb_set");

   -- Fonctions batterie (niveau bas)
   function Mrpiz_Battery_Voltage_Raw return C_float;
   pragma Import (C, Mrpiz_Battery_Voltage_Raw, "mrpiz_battery_voltage");

   function Mrpiz_Battery_Level_Raw return int;
   pragma Import (C, Mrpiz_Battery_Level_Raw, "mrpiz_battery_level");

   -- Interface de haut niveau avec gestion d'erreur
   
   -- Initialisation et fermeture
   procedure Mrpiz_Init;
   procedure Mrpiz_Close;

   -- Moteurs
   procedure Mrpiz_Motor_Set (Id : Mrpiz_Motor_Id; Cmd : Motor_Command_Percent);
   function Mrpiz_Motor_Encoder_Get (Id : Mrpiz_Motor_Id) return int;
   procedure Mrpiz_Motor_Encoder_Reset (Id : Mrpiz_Motor_Id);

   -- Capteurs de proximité
   function Mrpiz_Proxy_Sensor_Get (Id : Mrpiz_Proxy_Sensor_Id) return Sensor_Value;

   -- LED RGB
   procedure Mrpiz_Led_Rgb_Set (Color : Mrpiz_Led_Rgb_Color_T);

   -- Batterie
   function Mrpiz_Battery_Voltage return Float;
   function Mrpiz_Battery_Level return Battery_Level_Percent;

   -- Fonctions utilitaires
   procedure Stop_All_Motors;
   function Is_Obstacle_Detected (Threshold : Sensor_Value := 100) return Boolean;

end Mrpiz_Binding; 