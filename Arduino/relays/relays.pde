#include <SPI.h>
#include <Ethernet.h>

// MAC address for the Arduino
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };

// IP address for the Arduino
byte ip[] = { 192, 168, 1, 2 };

// IP address of the server to report to
byte server[] = { 192, 168, 1, 1 }; 

// Port number to connect to the server on
int port = 8181;

// Number of digital outputs
int pins = 4;

// Specify output pins in order
int pin[] = { 9, 8, 7, 6 };

// Output state, one 0 per state
int state[] = { 0, 0, 0, 0 };

// Here there be dragons...

Client client( server, port );

/**
 * Setup
 */
void setup()
{
  Serial.begin( 115200 );
  Ethernet.begin( mac, ip );
  
  // Allow ethernet shied time to initialise
  delay( 1000 );
  
  Serial.println( "connecting..." );

  if ( !client.connect() )
  {
    Serial.println( "connection failed" );
    
    while ( true )
    {
      continue;
    }
  }
  
  // Configure outputs and write initial state
  for ( int i = 0; i < pins; i++ )
  {
    pinMode( pin[i], OUTPUT );
    digitalWrite( pin[i], state[i] );
  }
  
  client.println( "DEVICE: relayboard" );
  client.println( "PORTS: relay1:O:D,relay2:O:D,relay3:O:D,relay4:O:D" );
  client.println( "DONE" );
  
  Serial.println( "Done" );
  
  printState();
}

/**
 * Loop
 */
void loop()
{
  if ( client.available() )
  {
    // Buffers
    char cmdBuf[ 8 ];
    int  cmdBufIdx = 0;
    char dataBuf[ 128 ];
    int  dataBufIdx = 0;
    byte theByte;
    
    theByte = client.read();
    
    // Read a command header
    while ( theByte != ':' )
    {
      cmdBuf[ cmdBufIdx ] = theByte;
      cmdBufIdx++;
      theByte = client.read();
    }
    
    // Terminate command string
    cmdBuf[ cmdBufIdx ] = '\0';
    
    // Don't care about the colon, read next byte
    theByte = client.read();
    
    // Read the command arguments
    while ( client.available() && theByte != '.' )
    {
      if ( theByte != ' ' )
      {
        dataBuf[ dataBufIdx ] = theByte;
        dataBufIdx++;
      }
      
      theByte = client.read();
    }
    
    // Terminate arguments string
    dataBuf[ dataBufIdx ] = '\0';
    
    // Print Command
    Serial.print( "> Command: " );
    Serial.println( cmdBuf );
    
    // Print Data
    Serial.print( "> Arguments: " );
    Serial.println( dataBuf );

    String command = String( cmdBuf );
    
    if ( command.equals( "VALUES" ) )
    {
      char argBuf[4];
      int  argBufIdx = 0;
      int  argIdx = 0;
      
      theByte = dataBuf[0];
      
      for ( int i = 0; i < dataBufIdx; i++ )
      {
        theByte = dataBuf[ i ];
        
        if ( theByte == ',' || theByte == 13 )
        {
          argBuf[ argBufIdx ] = '\0';
          
          String argument = String( argBuf );
         
          if ( argument.equals( "0" ) )
          {
            Serial.print( "Setting " );
            Serial.print( argIdx );
            Serial.println( " low." );
            
            state[ argIdx ] = LOW;
            digitalWrite( pin[ argIdx ], state[ argIdx ] );
          }
          else if ( argument.equals( "1" ) )
          {
             Serial.print( "Setting " );
            Serial.print( argIdx );
            Serial.println( " high." );
            
            state[ argIdx ] = HIGH;
            digitalWrite( pin[ argIdx ], state[ argIdx ] );
          }
          
          argBufIdx = 0;
          argIdx++;
        }
        else
        {
          argBuf[ argBufIdx ] = theByte;
          argBufIdx++;
        }
      }
    }
    else
    {
      client.println( "ERR: Invalid Command" );
    }
    
    printState();
  }
}

/**
 * Print State
 */
void printState()
{
  client.print( "VALUES: " );
  
  for ( int i = 0; i < pins; i++ )
  {
    client.print( state[ i ] );
    
    if ( ( i + 1 ) == pins )
    {
      client.println();
    }
    else
    {
      client.print( "," );
    }
  }
}
