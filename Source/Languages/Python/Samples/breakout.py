#Default Pico Explorer setup

import picoexplorer as display
import random
import utime as time
import picogfx
import machine
import math
import framebuf

from machine import Pin, I2C

#Set up display
width = display.get_width()
height = display.get_height()

#Setup I/O modules
display_buffer = bytearray(width * height * 2)  # 2-bytes per pixel (RGB565)
display.init(display_buffer)
gfx = picogfx.GFX(display.pixel, width, height)


#set some colours
black = display.create_pen(0,0,0)
white = display.create_pen(255,255,255)
red = display.create_pen(255,0,0)
l_red = display.create_pen(128,0,0)
green = display.create_pen(0,255,0)
blue = display.create_pen(0,0,255)
l_blue = display.create_pen(0,0,128)
yellow = display.create_pen(255,255,0)
l_ellow = display.create_pen(128,128,0)
cyan = display.create_pen(0,255,255)
l_cyan = display.create_pen(0,128,128)
magenta  = display.create_pen(255,0,255)
silver = display.create_pen(192,192,192)
gray = display.create_pen(128,128,128)
maroon = display.create_pen(128,0,0)
olive = display.create_pen(128,128,0)
l_green = display.create_pen(0,128,0)
purple = display.create_pen(128,0,128)
teal = display.create_pen(0,128,128)
l_blue = display.create_pen(0,0,128)
pink = display.create_pen(255,20,147)
none = ''

# setpen allows colours to be referenced by name
def displayclear():
    display.set_pen(black)
    display.clear()
    display.set_pen(white)
#Clear Screen
displayclear()
#######################################################

WIDTH = width
HEIGHT = height

display.set_pen(white)
lives = 5
speaker_pin = 1
bricks = []
# Create classes
class Sound():
    def __init__(self,speaker_pin):
        self.speaker = machine.PWM(machine.Pin(speaker_pin))
        self.speaker.duty_u16(0)
        
    def miss(self):
        self.speaker.duty_u16(int(65535/2))
        self.speaker.freq(220)
        self.timer = machine.Timer()
        self.timer.init(period=10, mode=machine.Timer.ONE_SHOT, callback=self.off)

    def hit(self):
        self.speaker.duty_u16(int(65535/2))
        self.speaker.freq(440)
        self.timer = machine.Timer()
        self.timer.init(period=10, mode=machine.Timer.ONE_SHOT, callback=self.off)
        
    def bounce(self):
        self.speaker.duty_u16(int(65535/2)) 
        self.speaker.freq(330)
        self.timer = machine.Timer()
        self.timer.init(period=10, mode=machine.Timer.ONE_SHOT, callback=self.off)

    def off(self, timer):
        self.speaker.duty_u16(0)
        
class Paddle():
    def __init__(self):
        self.x = WIDTH//2
        self.y = HEIGHT-4
        self.dx = 0
        self.width = 25
        self.height = 4
        self.score = 0

    #move with buttons
    def left(self):
        self.dx = -3
    
    def right(self):
        self.dx = 3
    
    def move(self):
        self.x = self.x + self.dx
        
        # Check for border collision
        if self.x < 0 + self.width//2:
            self.x = 0 + self.width//2
            self.dx = 0
        
        elif self.x > WIDTH - self.width//2:
            self.x = WIDTH - self.width//2
            self.dx = 0
    
    # move paddle with pot
    def moveadc(self):
        self.adc = display.get_adc(0)
        self.x = int((65535//(239-self.width//2))*self.adc)
        dx = 0
    
    #draw paddle
    def render(self):
        display.set_pen(white)
        display.rectangle(self.x-self.width//2, self.y-self.height//2, self.width, self.height) 

class Ball():
    def __init__(self):
        self.x = int(paddle.x)
        self.y = int(paddle.y-3)
        self.dx = 3
        self.dy = -3
        self.v1 = 1
        self.vy = 1
        self.speed =1
        self.width = 4
        self.height = 4
        
    def move(self):
        global lives
        self.x = self.x + self.dx
        self.y = self.y + self.dy
        
        # Check for border collision
        #has it hit left of screen
        if self.x < 0 + self.width//2:
            self.x = 0 + self.width//2
            self.dx *= -1
            sound.bounce()
        
        #has it hit right of screen
        elif self.x > WIDTH - self.width//2:
            self.x = WIDTH - self.width//2
            self.dx *= -1
            sound.bounce()
            
        #has it hit top of screen    
        if self.y < 15 + self.height//2:
            self.y = 15 + self.height//2
            self.dy *= -1
            sound.bounce()
            
        #has it hit bottom of screen
        elif self.y > HEIGHT - self.height//2:
            self.y = HEIGHT - self.height//2
            self.y = int(paddle.y - 4)
            self.x = paddle.x
            self.dy = -3
            self.dx = -3
            lives -=1
            sound.miss()
    def render(self):
        display.set_pen(white)
        display.rectangle(self.x-self.width//2, self.y-self.height//2, self.width, self.height) 

    def is_aabb_collision(self, other):
        # Axis Aligned Bounding Box
        x_collision = (math.fabs(self.x - other.x) * 2) < (self.width + other.width)
        y_collision = (math.fabs(self.y - other.y) * 2) < (self.height + other.height)
        return (x_collision and y_collision)

class Brick():
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.width = 22
        self.height = 7
        
    def render(self):
        display.set_pen(self.color)
        display.rectangle(self.x-self.width//2, self.y-self.height//2, self.width, self.height) 
        display.set_pen(white)
    
    def createwall():
        global bricks
        bricks = []
        for y in range(20, 85, 10):
            color = random.choice([red, white, green, yellow, blue, purple, cyan, magenta])
            for x in range(7, 240, 24):
                bricks.append(Brick(x, y))
                bricks[-1].color = color

def calculateangle(padX,padY,ballX):
    #calculate bounce angle off paddle
    #nearer center more virtical
    dx = ballX - (padX-12) 
    valTeta = (180/paddle.width)*dx
    vx=int(100*math.cos(math.radians(valTeta)))
    vy=int(100*math.sin(math.radians(valTeta)))
    vx = int(vx/15)
    vy = int(vy/15)
    if vy <2: vy = 2
    if vx <2: vx = 2
    print("dx: ",dx, "vx: ", vx, "vy: ",vy,"angle: ", valTeta)
    sound.hit()
    return vx * -1, vy * -1

def gameover():

    global lives, gameover
        #when lives are 0 game over

    flash = 1
    i = time.ticks_ms()
    gameover = 1
    while gameover:
        # timer loop to flash game over
        a = time.ticks_diff(time.ticks_ms(), i) 
        if a > 700:
            flash = flash * -1
            i = time.ticks_ms()
        if flash == -1:
            display.set_pen(red)
            display.rectangle(20,100,200,40)
            display.set_pen(black)
            display.text('game over', 25,105,240, 4)
        if flash == 1:
            display.set_pen(black)
            display.rectangle(20,100,200,40)
            display.set_pen(red)
            display.text('game over', 20,105,240, 4)
        display.set_pen(yellow)
        display.text('Press a key to    start',35,150,200,3)
        
        if display.is_pressed(display.BUTTON_Y) or display.is_pressed(display.BUTTON_B) :
            lives = 5
            paddle.score = 0
            gameover = 0
            Brick.createwall()
        ball.y = int(paddle.y - 4)
        ball.x = int(paddle.x-3)
        display.set_pen(black)
        display.rectangle(0,220,240,19)
        paddle.moveadc()
        paddle.move()
        ball.move()
        paddle.render()
        ball.render()
        display.update()
# Create game objects
paddle = Paddle()
ball = Ball()
sound = Sound(speaker_pin)
Brick.createwall()
for brick in bricks:
    brick.render()
display.set_pen(white)
# Main game loop
gameover()
while True:
    while lives:
        display.set_pen(black)
        display.clear()
        display.set_pen(white)
        
        # Keyboard events 
        ############################################
        # enable next 4 lines to use buttons to move
            
        #if display.is_pressed(display.BUTTON_B):
        #    paddle.left()
        #elif display.is_pressed(display.BUTTON_Y):
        #    paddle.right()
        
        
        ############################################
        # comment out this line if using buttons
        paddle.moveadc()
        ############################################
        
        # Update objects
        paddle.move()
        ball.move()
        
        #Check for collisions
        if ball.is_aabb_collision(paddle):
            ball.dx, ball.dy = calculateangle(paddle.x, paddle.y, ball.x)
            
        
        #remove dead bricks
        dead_bricks = []
        for brick in bricks:
            if ball.is_aabb_collision(brick):
                ball.dy *= -1
                dead_bricks.append(brick)
                paddle.score += 10
                sound.hit()
                
        for brick in dead_bricks:
            bricks.remove(brick)
        # redraw wall at end of level    
        if len(bricks) <= 0:
            Brick.createwall()
            paddle.score +=100
            lives +=1
            ball.y = int(paddle.y - 4)
            ball.x = int(paddle.x)
            ball. dy = -3
        # Render (Draw stuff)
        # Render objects
        paddle.render()
        ball.render()
        for brick in bricks:
            brick.render()
         
        # Render the lives
        # Render the score
        display.text(str(lives),5,0,2)
        display.text("Score:",80,0,2)
        display.text(str(paddle.score), 180,0,2)
        
        # update the display
        display.update()
    gameover()   
    