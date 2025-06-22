-------------------------------------------------
-- Binding Ada pour la bibliothèque mrpiz     --
-- Robot MRPiZ                                 --
-------------------------------------------------

with Interfaces.C;

package Mrpiz_Binding is

   use Interfaces.C;

   -- Constantes
   MRPIZ_ENCODE_PER_TURN : constant := 390;

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

   -- Fonctions d'initialisation et fermeture
   function Mrpiz_Init return int;
   pragma Import (C, Mrpiz_Init, "mrpiz_init");

   procedure Mrpiz_Close;
   pragma Import (C, Mrpiz_Close, "mrpiz_close");

   -- Fonctions des moteurs
   function Mrpiz_Motor_Set (Id : Mrpiz_Motor_Id; Cmd : int) return int;
   pragma Import (C, Mrpiz_Motor_Set, "mrpiz_motor_set");

   function Mrpiz_Motor_Encoder_Get (Id : Mrpiz_Motor_Id) return int;
   pragma Import (C, Mrpiz_Motor_Encoder_Get, "mrpiz_motor_encoder_get");

   function Mrpiz_Motor_Encoder_Reset (Id : Mrpiz_Motor_Id) return int;
   pragma Import (C, Mrpiz_Motor_Encoder_Reset, "mrpiz_motor_encoder_reset");

   -- Fonctions des capteurs de proximité
   function Mrpiz_Proxy_Sensor_Get (Id : Mrpiz_Proxy_Sensor_Id) return int;
   pragma Import (C, Mrpiz_Proxy_Sensor_Get, "mrpiz_proxy_sensor_get");

   -- Fonctions LED RGB
   function Mrpiz_Led_Rgb_Set (Color : Mrpiz_Led_Rgb_Color_T) return int;
   pragma Import (C, Mrpiz_Led_Rgb_Set, "mrpiz_led_rgb_set");

   -- Fonctions batterie
   function Mrpiz_Battery_Voltage return C_float;
   pragma Import (C, Mrpiz_Battery_Voltage, "mrpiz_battery_voltage");

   function Mrpiz_Battery_Level return int;
   pragma Import (C, Mrpiz_Battery_Level, "mrpiz_battery_level");

end Mrpiz_Binding; 