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

// Number of analog inputs to read
int inputs = 6;

// Specify analog inputs in order
int input[] = { A0, A1, A2, A3, A4, A5 };

// Input state, one 0 per state
int state[] = { 0, 0, 0, 0, 0, 0 };

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
    Serial.println( "connection failed " );
    
    while ( true )
    {
      continue;
    }
  }
  
  client.println( "DEVICE: arduinosensors proto2" );
  client.println( "PORTS: ldr1:I:A:0-1024,ldr2:I:A:0-1024,therm:I:A:0-1024,aux1:I:A:0-1024,aux2:I:A:0-1024,aux3:I:A:0-1024" );
  client.println( "DONE" );
  
  Serial.println( "Done" );
}

/**
 * Loop
 */
void loop()
{
  readInputs();
  printState();
  delay( 1000 );
}

// Read the specified analog inputs and update the state array
// with their values
void readInputs()
{
  for ( int i = 0; i < inputs; i++ )
  {
    state[ i ] = analogRead( input[ i ] );
  }
}

// Print the contents of the state array to the TCP client as CSV
void printState()
{
  client.print( "VALUES: " );

  for ( int i = 0; i < inputs; i++ )
  {
    client.print( state[ i ] );
    
    if ( ( i + 1 ) == inputs )
    {
      client.println();
    }
    else
    {
      client.print( "," );
    }
  }
}
