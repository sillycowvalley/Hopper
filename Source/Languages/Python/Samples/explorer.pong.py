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
Black = (0,0,0)
White = (255,255,255)
Red = (255,0,0)
DimRed = (128,0,0)
Green = (0,255,0)
Blue = (0,0,255)
DimBlue = (0,0,128)
Yellow = (255,255,0)
DimYellow = (128,128,0)
Cyan = (0,255,255)
DimCyan = (0,128,128)
Magenta  = (255,0,255)
Silver = (192,192,192)
Gray = (128,128,128)
Maroon = (128,0,0)
Olive = (128,128,0)
DimGreen = (0,128,0)
Purple = (128,0,128)
Teal = (0,128,128)
DimBlue = (0,0,128)
Pink = (255,20,147)
none = ''

# setpen allows colours to be referenced by name
def setpen (colour):
    display.set_pen(colour[0], colour[1], colour[2])
def displayclear():
    setpen(Black)
    display.clear()
    setpen(White)
#Clear Screen
displayclear()
print("GurgleApps.com Pico Pong")
# would have used pin 1 & 2 but they were broken on one of our Pico Boards
speaker_pin = 1
play_button_pin = 2
left = 27
right = 26
clockPin = 5
dataPin = 4
bus = 0
mode_init = 1
mode_playing = 2
mode_game_over = 3
mode = mode_init # mode_init,mode_playing,mode_game_over
analog_pin_left = machine.ADC(left)
analog_pin_right = machine.ADC(right)
play_button = machine.Pin(play_button_pin,machine.Pin.IN, machine.Pin.PULL_UP)
speaker = machine.PWM(machine.Pin(speaker_pin))
speaker.duty_u16(0)
clean_count = 0
analog_min = 380
analog_max = 65535
paddle_l_y_old = -1
paddle_r_y_old = -1
max_score = 10
ball_speed = 6
ball_width = 5
ball_height = 5
ball_half_height = ball_height >> 1
ball_x = 0
ball_y = 0
ball_vx = ball_speed
ball_vy = ball_speed
max_x = 239
max_y = 239
min_x = 0
min_y = 0
paddle_width = 6
paddle_height = 24
paddle_half_height = paddle_height >> 1
paddle_x = 128 - paddle_width
paddle_y = 30
paddle_speed = 5
net_width = 2
net_segmment_height = 20
net_segmment_gap = 10
l_score = 0
r_score = 0
last_up = time.ticks_ms()
#buff = framebuf.FrameBuffer(bytearray(max_x*max_y),max_x,max_y,framebuf.MONO_HLSB)

def play_theme():
    notes = [[587,0.5],[523,0.5],[587,0.5],[0,0.5],[587,0.5],[523,0.5],[587,0.5],[0,0.5]
             ,[587,0.5],[523,0.5],[440,0.5],[523,0.5],[659,0.5],[523,0.5],[587,0.75]]
    for note in notes:
        speaker.duty_u16(int(65535/2))
        if note[0] == 0:
            speaker.duty_u16(0)
        else:
            speaker.freq(note[0])
        time.sleep(note[1])
    speaker.duty_u16(0)

def draw_net():
    y=0
    while y < max_y:
        gfx.fill_rect((max_x-net_width)>>1,y,net_width,net_segmment_height)
        y+=net_segmment_height + net_segmment_gap

def roundUp(x):
    return ((x+7)&(-8))

# def dataToBuff(data):
#     height = len(data)
#     width = len(data[0])
#     height = roundUp(height)
#     width = roundUp(width)
#     fbuf = framebuf.FrameBuffer(bytearray(int(width * height / 8)), width, height, framebuf.MONO_HLSB)
#     for y, row in enumerate(data):
#         for x, c in enumerate(row):
#             fbuf.pixel(x,y,c)
#     return fbuf

  
"""
Change value of analog pin to y position
"""
def analog_to_y(analog_pin):
    analog = analog_pin.read_u16()
    if analog < analog_min:
        analog = analog_min
    if analog > analog_max:
        analog = analog_max
    analog = analog - analog_min
    analog = analog / (analog_max - analog_min)
    analog= int(analog * (max_y - paddle_height))
    return analog

def sound_miss():
    speaker.duty_u16(int(65535/2))
    speaker.freq(220)
    timer = machine.Timer()
    timer.init(freq=2, mode=machine.Timer.ONE_SHOT, callback=sound_off)
    
def sound_hit():
    speaker.duty_u16(int(65535/2))
    speaker.freq(440)
    timer = machine.Timer()
    timer.init(freq=20, mode=machine.Timer.ONE_SHOT, callback=sound_off)
    
def sound_bounce():
    speaker.duty_u16(int(65535/2))
    speaker.freq(330)
    timer = machine.Timer()
    timer.init(freq=20, mode=machine.Timer.ONE_SHOT, callback=sound_off)

def sound_off(timer):
    speaker.duty_u16(0)

def point_to(player):
    global l_score,r_score, mode
    sound_miss()
    if player == "left":
        l_score+=1
        gfx.fill_rect(0,0,120,15)
        setpen(Black)
        display.text(str(l_score),30,0,2)
        setpen(White)
        display.update()
        if l_score == max_score:
            mode = mode_game_over
            setpen(White)
            display.text("WIN!",30,115,3)
            display.update()
            time.sleep(0.5)
            play_theme()
    elif player == "right":
        r_score+=1
        gfx.fill_rect(120,0,120,15)
        setpen(Black)
        display.text(str(r_score),190,0,2)
        setpen(White)
        display.update()
        if r_score == max_score:
            mode = mode_game_over
            setpen(White)
            display.text("WIN!",150,115,3)
            display.update()
            time.sleep(0.5)
            play_theme()
    
    
def intro():
    global l_score, r_score
    setpen(White)
    
    l_score = 0
    r_score = 0
    display.text(str(l_score),30,0,2)
    display.text(str(r_score),190,0,2)
    clean_count = 0
    
def play_frame():
    global clean_count, paddle_l_y_old, paddle_r_y_old,ball_x,ball_y,ball_vx,ball_vy
    displayclear()
    display.text(str(l_score),30,0,2)
    display.text(str(r_score),190,0,2)
    draw_net()

    paddle_l_y = analog_to_y(analog_pin_left)
    paddle_r_y = analog_to_y(analog_pin_right)
    clean_count = (clean_count + 1) % 6
    if clean_count == 0:
        pass
      
    gfx.fill_rect(0,paddle_l_y,paddle_width,paddle_height) #left paddle
    gfx.fill_rect(max_x - paddle_width,paddle_r_y,paddle_width,paddle_height) #left paddle

        
    ball_x += int(ball_vx)
    ball_y += int(ball_vy)
    if ball_y > max_y - ball_height:
        sound_bounce()
        ball_vy = -ball_vy
        ball_y = max_y - ball_height
    elif ball_y < min_y:
        sound_bounce()
        ball_vy = -ball_vy
        ball_y = min_y
    if ball_x >= max_x - ball_width: #hit right paddle
        dy = (ball_y + ball_height) - paddle_r_y
        if dy > 0 and dy < paddle_height + ball_height:
            sound_hit()
            dy = dy / (paddle_height+ball_height)
            ball_x = max_x - ball_width
            angle = math.radians(90-60+dy*120)
            ball_vx = -math.sin(angle) * ball_speed
            ball_vy = -math.cos(angle) * ball_speed
            print("dy: ",dy, "vx: ", ball_vx)
        else:
            point_to("left")
            ball_x = max_x >> 1
            ball_y = max_y >> 1
            ball_vx = -ball_vx
    elif ball_x <= min_x + paddle_width:
        dy = (ball_y + ball_height) - paddle_l_y
        if dy > 0 and dy < paddle_height + ball_height:
            sound_hit()
            dy = dy / (paddle_height+ball_height)
            ball_x = min_x + paddle_width
            angle = math.radians(90-60+dy*120)
            ball_vx = math.sin(angle) * ball_speed
            ball_vy = -math.cos(angle) * ball_speed
        else:
            point_to("right")
            ball_x = max_x >> 1
            ball_y = max_y >> 1
            ball_vx = -ball_vx
    gfx.fill_rect(int(ball_x),int(ball_y),ball_width,ball_height)
    display.update()
    time.sleep(0.05)

while True:
    if mode == mode_playing:
        play_frame()
    elif mode == mode_init:
        intro()
        mode = mode_game_over
    elif mode == mode_game_over:
        time.sleep(0.1)
        if not play_button.value():
            intro()
            mode=mode_playing
    


