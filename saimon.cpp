#define NONE -1

byte sequence[256];
byte ledPins[] = {2,3,4,5};
byte buttonPins[] =  {6,7,8,9};
byte size = 0;
byte pointer = 0;
void setup()
{
    randomSeed(analogRead(0));

    for (int i = 0; i < sizeof(ledPins); i++)
    {
        pinMode(ledPins[i], OUTPUT);
        digitalWrite(ledPins[i], LOW);
        pinMode(buttonPins[i], INPUT_PULLUP);
    }
    
    sajmonInit();
}

void loop()
{

    if (pointer == size)
    {
        pointer = 0;
        int newColor = addRandomColor();
        blinkAll();
        showSequence();
    }
    else
    {
        init();
        blinkAll();
    }

    int pressed = NONE;
    while (pressed == NONE)
    {
        pressed = getPressed();
        delay(25);
    }


    blinkOne(ledPins[pressed]);
}

void showSequence()
{
    for (int i = 0; i < size; i++)
    {
        blinkOne(sequence[i]);
    }
}

int getPressed()
{
    for (int i = 0; i < sizeof(buttonPins); i++)
    {
        if (digitalRead(buttonPins[i]) == LOW)
        {
            return i;
        }
    }
    return NONE;
}

void blinkOne(int diode)
{
    digitalWrite(diode, HIGH);
    delay(500);
    digitalWrite(diode, LOW);

}
void blinkAll()
{
    for (int j = 0; j < 12; j++)
    {
        for (int i = 0; i < sizeof(ledPins); i++)
        {
            digitalWrite(ledPins[i], digitalRead(ledPins[i]));
        }
        delay(100);
    }

}


int addRandomColor()
{
    sequence[size] = ledPins[random(0, sizeof(ledPins))];
    size += 1;
    return sequence[size - 1];
}

void sajmonInit()
{
    size = 0;
    pointer = 0;
}
