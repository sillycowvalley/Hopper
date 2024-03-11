# Original by Gary Metheringham for Pimoroni Pico Explorer
# Adapted for SSD1306 OLED by Hari Wiguna
# Connect 128x64 OLED SDA to GPIO 2, SCL to GPIO 3
# Pushbuttons are on GPIO 12,13,14, and 15 (see setupIO below) to ground. Uses internal pullup resistor.

#== Imports ==
import time, random
from machine import Pin, I2C
from ssd1306 import SSD1306_I2C

#== Globals ==
#-- OLED --
BUS,SDA,SCL = 1, 2,3
WIDTH, HEIGHT = 128, 64

#-- Game Arena --
snakeblock = 4
border=2
xMin=border
yMin=9+border # 9 is text area for title and score
xMax=WIDTH-border-snakeblock # Note that xMax and yMax is a snakeblock less than actual
yMax=HEIGHT-border-snakeblock
arenaWidth = xMax-xMin # Note that arenaWidth and arenaHeight is a snakeblock less than actual
arenaHeight = yMax-yMin

def setupOled():
    global oled
    i2c = I2C(BUS, sda=Pin(SDA), scl=Pin(SCL), freq=400000)
    oled = SSD1306_I2C(WIDTH, HEIGHT, i2c)
    oled.fill(0)

def setupIO():
    global buttonRight, buttonLeft, buttonUp, buttonDown
    buttonRight = Pin(13, Pin.IN, Pin.PULL_UP)
    buttonLeft = Pin(14, Pin.IN, Pin.PULL_UP)
    buttonUp = Pin(15, Pin.IN, Pin.PULL_UP)
    buttonDown = Pin(12, Pin.IN, Pin.PULL_UP)

# Printing Title and draw screen
def draw_frame():
    oled.fill(0)
    oled.text(str(length_of_snake-1), 100,0)  
    oled.text('SNAKE',0,0)
    oled.rect(xMin-border,yMin-border, xMax-xMin+snakeblock+2*border,yMax-yMin+snakeblock+2*border, 1)
    oled.rect(xMin,yMin, xMax-xMin+snakeblock,yMax-yMin+snakeblock, 1)
   
def game_over():
    oled.text('Game Over',30,int(HEIGHT/2), 1)
    oled.show()

def wait_for_any_buttons():
    while buttonRight.value()==1 and \
          buttonLeft.value()==1 and \
          buttonUp.value()==1 and \
          buttonDown.value()==1:
        time.sleep(.1)

def reset():
    global x1
    x1 = xMin + int(arenaWidth/2/snakeblock)*snakeblock
    
    global y1
    y1 = yMin + int(arenaHeight/2/snakeblock)*snakeblock
    
    make_food()
    
    global x1_change
    x1_change = 1
    
    global y1_change
    y1_change = 0
    
    global snake_list
    snake_list = []
    
    global length_of_snake
    length_of_snake = 3
    
    global snake
    snake = [[x1,y1], [x1,y1-snakeblock], [x1,y1-snakeblock*2]]
    
    global is_game_over
    is_game_over = False

def make_food():
    global food
    food = [ xMin + random.randrange(int(arenaWidth/snakeblock))*snakeblock , yMin + random.randrange(int(arenaHeight/snakeblock))*snakeblock ]

def draw_food():
    oled.rect(food[0],food[1],snakeblock,snakeblock, 1)

def move_snake():
    global x1,y1, x1_change, y1_change, snake_list, length_of_snake
    global is_game_over
    
    if buttonDown.value()==0:
        y1_change = snakeblock
        x1_change = 0
    elif buttonUp.value()==0:
        y1_change = -snakeblock
        x1_change = 0
    elif buttonRight.value()==0:
        y1_change = 0
        x1_change = snakeblock
    elif buttonLeft.value()==0:
        y1_change = 0
        x1_change = -snakeblock

    x1 += x1_change
    y1 += y1_change
    if x1 > xMax or x1 < xMin or y1 > yMax or y1 < yMin:
        is_game_over = True

def draw_snake():
    global snake_list, length_of_snake, food, is_game_over
    
    snakehead = [x1,y1]
    snake_list.append(snakehead)
    if len(snake_list) > length_of_snake:
        del snake_list[0]
    for x in snake_list[:-1]:
        if x == snakehead:
            is_game_over = True
    for x in snake_list:
        oled.rect(x[0],x[1],snakeblock,snakeblock, 1)    
    # crash detection
    if (x1 == food[0] and y1 == food[1]):
        make_food()
        length_of_snake +=1

def main():
    setupOled()
    setupIO()

    while True:
        reset()
        draw_frame()
        oled.text('Press to Start',8,int(HEIGHT/2), 1)
        oled.show()
        wait_for_any_buttons()

        while not is_game_over:
            draw_frame()
            draw_food()
            move_snake()
            draw_snake()
            oled.show()
            time.sleep_ms(100)
            
        game_over()
        wait_for_any_buttons()
