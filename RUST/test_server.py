#!/usr/bin/env python3
"""
Quick test script for MRPiZ server
Tests different endpoints and network messages of the robot
"""

import socket
import struct
import time
import sys
from enum import IntEnum
from typing import Optional, List, Tuple

class NetworkMessageType(IntEnum):
    """Network message types according to MRPiZ protocol"""
    # SENT BY ANDROID
    ID_SET_MOVEMENT = 0x01
    ID_MISSION_CONTROL = 0x02
    ID_SELECTED_POINTS = 0x05
    ID_UPLOAD_MAP = 0x07
    
    # SENT BY BOT
    ID_INF_BATTERY = 0x0A
    ID_INF_STATUS = 0x0B
    ID_INF_POS = 0x0C
    ID_INF_TIME = 0x0D
    
    ID_MAP_FRAGMENT = 0x20
    ID_MAP_FULL = 0x21
    
    # UDP
    ID_IS_ANY_ROBOT_HERE = 0x30
    ID_MANIFEST = 0x31

class MRPiZTester:
    def __init__(self, host: str = "127.0.0.1", port: int = 8080):
        self.host = host
        self.port = port
        self.socket = None
        
    def connect(self) -> bool:
        """Establishes connection with the server"""
        try:
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(10.0)  # 10 seconds timeout
            self.socket.connect((self.host, self.port))
            print(f"✅ Connection established with {self.host}:{self.port}")
            return True
        except Exception as e:
            print(f"❌ Connection error: {e}")
            return False
    
    def disconnect(self):
        """Closes the connection"""
        if self.socket:
            self.socket.close()
            self.socket = None
            print("🔌 Connection closed")
    
    def create_message(self, msg_type: NetworkMessageType, data: List[int]) -> bytes:
        """
        Creates a message according to MRPiZ protocol
        Format: [length:u16][idx:u8][data:bytes]
        """
        # length = 1 (idx) + len(data)
        length = 1 + len(data)
        
        # Encode in little-endian
        message = struct.pack('<H', length)  # length (u16)
        message += struct.pack('B', msg_type.value)  # idx (u8)
        message += bytes(data)  # data
        
        return message
    
    def send_message(self, msg_type: NetworkMessageType, data: List[int]) -> Optional[bytes]:
        """Sends a message and returns the response"""
        if not self.socket:
            print("❌ No active connection")
            return None
        
        try:
            message = self.create_message(msg_type, data)
            print(f"📤 Sending message {msg_type.name}: {len(message)} bytes")
            print(f"   Data: {data}")
            
            self.socket.send(message)
            
            # Receive response
            response = self.socket.recv(1024)
            print(f"📥 Response received: {len(response)} bytes")
            print(f"   Content: {response.decode('utf-8', errors='ignore')}")
            
            return response
            
        except Exception as e:
            print(f"❌ Error during send: {e}")
            return None
    
    def test_movement_commands(self):
        """Tests movement commands"""
        print("\n🚗 Testing movement commands")
        print("=" * 40)
        
        movements = [
            (1, 50, "Move forward speed 50"),
            (2, 30, "Move backward speed 30"),
            (3, 40, "Turn left speed 40"),
            (4, 60, "Turn right speed 60"),
            (0, 0, "Stop"),
        ]
        
        for direction, speed, description in movements:
            print(f"\n🎯 {description}")
            self.send_message(NetworkMessageType.ID_SET_MOVEMENT, [direction, speed])
            time.sleep(0.5)
    
    def test_robot_discovery(self):
        """Tests robot discovery"""
        print("\n🔍 Testing robot discovery")
        print("=" * 40)
        
        self.send_message(NetworkMessageType.ID_IS_ANY_ROBOT_HERE, [])
    
    def test_mission_control(self):
        """Tests mission control commands"""
        print("\n🎮 Testing mission control")
        print("=" * 40)
        
        # Test with different mission commands
        mission_commands = [
            [1, 0, 0, 0],  # Start mission
            [2, 0, 0, 0],  # Pause mission
            [3, 0, 0, 0],  # Stop mission
        ]
        
        for i, cmd in enumerate(mission_commands):
            action = ["Start", "Pause", "Stop"][i]
            print(f"\n📋 {action} mission")
            self.send_message(NetworkMessageType.ID_MISSION_CONTROL, cmd)
            time.sleep(0.5)
    
    def test_map_upload(self):
        """Tests map upload"""
        print("\n🗺️ Testing map upload")
        print("=" * 40)
        
        # Simulate map data (example)
        map_data = [0x12, 0x34, 0x56, 0x78, 0xAB, 0xCD]
        self.send_message(NetworkMessageType.ID_UPLOAD_MAP, map_data)
    
    def test_selected_points(self):
        """Tests sending selected points"""
        print("\n📍 Testing selected points")
        print("=" * 40)
        
        # Point coordinates (x1, y1, x2, y2)
        points = [100, 200, 150, 250, 300, 400]
        self.send_message(NetworkMessageType.ID_SELECTED_POINTS, points)
    
    def run_basic_tests(self):
        """Runs a basic test suite"""
        print("🧪 STARTING MRPiZ SERVER TESTS")
        print("=" * 50)
        
        if not self.connect():
            return False
        
        try:
            # Main tests
            self.test_robot_discovery()
            time.sleep(1)
            
            self.test_movement_commands()
            time.sleep(1)
            
            self.test_mission_control()
            time.sleep(1)
            
            self.test_selected_points()
            time.sleep(1)
            
            self.test_map_upload()
            
            print("\n✅ All tests completed successfully!")
            return True
            
        except KeyboardInterrupt:
            print("\n⚠️ Tests interrupted by user")
            return False
        except Exception as e:
            print(f"\n❌ Error during tests: {e}")
            return False
        finally:
            self.disconnect()
    
    def interactive_mode(self):
        """Interactive mode for manual testing"""
        print("\n🎮 INTERACTIVE MODE")
        print("=" * 30)
        print("Available commands:")
        print("  1. Test movement")
        print("  2. Robot discovery")
        print("  3. Mission control")
        print("  4. Selected points")
        print("  5. Map upload")
        print("  6. Complete tests")
        print("  q. Quit")
        
        if not self.connect():
            return
        
        try:
            while True:
                choice = input("\n🎯 Your choice: ").strip().lower()
                
                if choice == 'q':
                    break
                elif choice == '1':
                    self.test_movement_commands()
                elif choice == '2':
                    self.test_robot_discovery()
                elif choice == '3':
                    self.test_mission_control()
                elif choice == '4':
                    self.test_selected_points()
                elif choice == '5':
                    self.test_map_upload()
                elif choice == '6':
                    self.disconnect()
                    self.run_basic_tests()
                    if not self.connect():
                        break
                else:
                    print("❌ Invalid choice")
                    
        except KeyboardInterrupt:
            print("\n⚠️ Interactive mode interrupted")
        finally:
            self.disconnect()

def test_connection_only(host: str = "127.0.0.1", port: int = 8080):
    """Simple connection test"""
    print(f"🔍 Quick connection test on {host}:{port}")
    
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(5.0)
        result = sock.connect_ex((host, port))
        sock.close()
        
        if result == 0:
            print("✅ Server accessible!")
            return True
        else:
            print("❌ Server inaccessible")
            return False
            
    except Exception as e:
        print(f"❌ Test error: {e}")
        return False

def main():
    """Main function"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Tester for MRPiZ server")
    parser.add_argument("--host", default="127.0.0.1", help="Server address")
    parser.add_argument("--port", type=int, default=8080, help="Server port")
    parser.add_argument("--interactive", "-i", action="store_true", help="Interactive mode")
    parser.add_argument("--test-connection", "-t", action="store_true", help="Connection test only")
    
    args = parser.parse_args()
    
    if args.test_connection:
        test_connection_only(args.host, args.port)
        return
    
    tester = MRPiZTester(args.host, args.port)
    
    if args.interactive:
        tester.interactive_mode()
    else:
        tester.run_basic_tests()

if __name__ == "__main__":
    main() 