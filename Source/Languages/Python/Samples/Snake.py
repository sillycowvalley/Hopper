
import machine
import time, random
import picoexplorer as explorer
import joystick

stick = joystick.Joystick()
width = explorer.get_width()
height = explorer.get_height()
#set some Varibles
height = 240
width = 240
xpos= 117
ypos = 134
score = 0
lastpos= 0
food = 0
snakeblock = 8
length_of_snake = 1
display_buffer = bytearray(width * height * 2)  # 2-bytes per pixel (RGB565)
explorer.init(display_buffer)
# Printing Title and draw screen
def screen():
    explorer.set_pen(0, 0, 0)
    explorer.clear()
    explorer.set_pen(0, 180, 0)
    explorer.text('Score : ', 100,0,240,2)
    global length_of_snake
    explorer.text(str(length_of_snake-1), 180,0,240,2)  
    explorer.text('SNAKE',0,0,240,2)
    explorer.set_pen(200, 200, 200)
    explorer.rectangle(0,16,240,234)
    explorer.set_pen(0, 0, 0)
    explorer.rectangle(2,18,236,220)

    
#Game Over
def game_over():
    a = stick.digitalstick()
    while a is not 16:
        explorer.set_pen(200, 200, 200)
        explorer.text('Game Over',65,90,100,5)
        explorer.update()
        a = stick.digitalstick() & 16
    reset()
    
def reset():
    global x1
    global width
    x1 = width/2
    global y1
    global height
    y1 = ((height-20)/2)+20
    global score
    score = 0
    global lastpos
    lastpos = 0
    global food
    global width
    global height
    food = [ random.randint(1, (width-6))+2 , random.randint(1, (height-22))+20]                                                 # First food co-ordinates
    global x1_change
    x1_change = 0
    global y1_change
    y1_change = 0
    global snake_list
    snake_list = []
    global length_of_snake
    length_of_snake = 1
    global snake
    global height
    global width
    snake = [[height/2,width/2], [height/2,(width/2)-7], [height/2,(width/2)-14]] 
reset()
gameover = False
while True:
    screen()
    

    
    explorer.set_pen(200, 0, 0)
    explorer.rectangle(food[0],food[1],snakeblock,snakeblock)
    
        
    key= stick.digitalstick()

    if key & 1:
        y1_change = snakeblock
        x1_change = 0
    elif key & 2:
        y1_change = -snakeblock
        x1_change = 0
    elif key & 4:
        y1_change = 0
        x1_change = snakeblock
    elif key & 8:
        y1_change = 0
        x1_change = -snakeblock
    elif key & 16:
        y1_change = 0
        x1_change = 0
    if x1 >= 232 or x1 <= -1 or y1 >= 234 or y1 <= 17:
        game_over()
    x1 += x1_change
    y1 += y1_change
    x1 = int(x1)
    y1 = int(y1)
    #draw snake
    snakehead = []
    snakehead.append(x1)
    snakehead.append(y1)
    snake_list.append(snakehead)
    if len(snake_list) > length_of_snake:
        del snake_list[0]
    for x in snake_list[:-1]:
        if x == snakehead:
            game_over()
    for x in snake_list:
        explorer.set_pen(200, 200, 200)
        explorer.rectangle(int(x[0]),int(x[1]),snakeblock,snakeblock)    
    # crash detection
    if (x1 >= food[0]-6 and x1<= food[0]+5) and (y1 >= food[1]-6 and y1 <= food[1]+5):
        food = [ random.randint(1, (width-14))+2 , random.randint(1, (height-24))+20]                                                 # First food co-ordinates
        length_of_snake +=1
        
    explorer.update()
    time.sleep_ms(100)


 
    