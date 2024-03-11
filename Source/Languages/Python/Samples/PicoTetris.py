import picoexplorer as display
import random

#Set up display
width = display.get_width()
height = display.get_height()
display_buffer = bytearray(width * height * 2)  # 2-bytes per pixel (RGB565)
display.init(display_buffer)

colors = [
    display.create_pen(0, 0, 0),
    display.create_pen(120, 37, 179), #purple
    display.create_pen(100, 179, 179), #cyan
    display.create_pen(100, 54, 42), #brown
    display.create_pen(80, 255, 22), # green
    display.create_pen(255, 34, 22), #red
    display.create_pen(22, 34, 255),#blue
    display.create_pen(255, 0, 255), #magenta
    display.create_pen(255, 255, 0),#yellow
    display.create_pen(0, 255, 255), 
]


class Figure:
    x = 0
    y = 0

    figures = [
        [[1, 5, 9, 13], [4, 5, 6, 7]],
        [[4, 5, 9, 10], [2, 6, 5, 9]],
        [[6, 7, 9, 10], [1, 5, 6, 10]],
        [[1, 2, 5, 9], [0, 4, 5, 6], [1, 5, 9, 8], [4, 5, 6, 10]],
        [[1, 2, 6, 10], [5, 6, 7, 9], [2, 6, 10, 11], [3, 5, 6, 7]],
        [[1, 4, 5, 6], [1, 4, 5, 9], [4, 5, 6, 9], [1, 5, 6, 9]],
        [[1, 2, 5, 6]],
    ]

    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.type = random.randint(0, len(self.figures) - 1)
        self.color = random.randint(1, len(colors) - 1)
        self.rotation = 0

    def image(self):
        return self.figures[self.type][self.rotation]

    def rotate(self):
        self.rotation = (self.rotation + 1) % len(self.figures[self.type])


class Tetris:
    level = 2
    score = 0
    state = "start"
    field = []
    height = 0
    width = 0
    x = 2
    y = 2
    zoom = 12
    figure = None

    def __init__(self, width, height):
        self.height = height
        self.width = width
        self.field = []
        self.score = 0
        self.state = "start"
        for i in range(height):
            new_line = []
            for j in range(width):
                new_line.append(0)
            self.field.append(new_line)

    def new_figure(self):
        self.figure = Figure(3, 0)

    def intersects(self):
        intersection = False
        for i in range(4):
            for j in range(4):
                if i * 4 + j in self.figure.image():
                    if i + self.figure.y > self.height - 1 or \
                            j + self.figure.x > self.width - 1 or \
                            j + self.figure.x < 0 or \
                            self.field[i + self.figure.y][j + self.figure.x] > 0:
                        intersection = True
        return intersection

    def break_lines(self):
        lines = 0
        for i in range(1, self.height):
            zeros = 0
            for j in range(self.width):
                if self.field[i][j] == 0:
                    zeros += 1
            if zeros == 0:
                lines += 1
                for i1 in range(i, 1, -1):
                    for j in range(self.width):
                        self.field[i1][j] = self.field[i1 - 1][j]
        self.score += lines ** 2

    def go_space(self):
        while not self.intersects():
            self.figure.y += 1
        self.figure.y -= 1
        self.freeze()

    def go_down(self):
        self.figure.y += 1
        if self.intersects():
            self.figure.y -= 1
            self.freeze()

    def freeze(self):
        for i in range(4):
            for j in range(4):
                if i * 4 + j in self.figure.image():
                    self.field[i + self.figure.y][j + self.figure.x] = self.figure.color
        self.break_lines()
        self.new_figure()
        if self.intersects():
            self.state = "gameover"

    def go_side(self, dx):
        old_x = self.figure.x
        self.figure.x += dx
        if self.intersects():
            self.figure.x = old_x

    def rotate(self):
        old_rotation = self.figure.rotation
        self.figure.rotate()
        if self.intersects():
            self.figure.rotation = old_rotation

def clear():  
    display.set_pen(0, 0, 0)
    display.clear()
# Printing Title and draw screen
def screen():
    clear()
    display.set_pen(0, 180, 0)
    display.text('Score', 140,100,240,3)
    
    display.text(str(game.score), 140,130,240,3)  
    display.text('Pico',150,25,240,3)
    display.text('TETRIS',135,50,240,3)
    display.set_pen(200, 200, 200)
    display.rectangle(0,0,124,226)
    display.set_pen(0, 0, 0)
    display.rectangle(2,2,120,220)

    
#Game Over
def game_over():
    counter = 0
    restart = False
    flash = True
    while not restart:
        if flash:
            display.set_pen(200, 200, 200)
        if not flash:
            display.set_pen(255, 0, 0)
            
        display.text('Game Over',65,90,100,5)
        if counter > 25:
            flash = not flash
            counter = 0
        counter +=1
        display.update()
        if display.is_pressed(display.BUTTON_X):
                restart = True
                
size = (width, height)

# Loop until the user clicks the close button.
done = False
fps = 25
game = Tetris(120//12, 220//12)
counter = 0

pressing_down = False

while True:
    screen()
    if game.figure is None:
        game.new_figure()
    counter += 1
    if counter > 100000:
        counter = 0

    if counter % (fps // game.level // 2) == 0 or pressing_down:
        if game.state == "start":
            game.go_down()
            pressing_down = False
            if display.is_pressed(display.BUTTON_X):
                game.rotate()
            if display.is_pressed(display.BUTTON_A):
                pressing_down = True
            if display.is_pressed(display.BUTTON_B):
                game.go_side(-1)
            if display.is_pressed(display.BUTTON_Y):
                game.go_side(1)
            #if event.key == pygame.K_SPACE:
                #game.go_space()
#draw play grid    
    for i in range(game.height):
        for j in range(game.width):
            display.set_pen(25,25,25)
            display.rectangle(game.x + game.zoom * j, game.y + game.zoom * i, game.zoom, game.zoom)
            display.set_pen(0,0,0)
            display.rectangle(game.x + game.zoom * j+1, game.y + game.zoom * i+1, game.zoom-1, game.zoom-1)
            
            
            if game.field[i][j] > 0:
                display.set_pen(colors[game.field[i][j]])
                display.rectangle(game.x + game.zoom * j + 1, game.y + game.zoom * i + 1, game.zoom - 2, game.zoom - 1)
# draw figure                 
    if game.figure is not None:
        for i in range(4):
            for j in range(4):
                p = i * 4 + j
                if p in game.figure.image():
                    display.set_pen(colors[game.figure.color])
                    display.rectangle(game.x + game.zoom * (j + game.figure.x) + 1, game.y + game.zoom * (i + game.figure.y) + 1, game.zoom - 2, game.zoom - 2) 
    if game.state == "gameover":
        game_over()
        game = Tetris(120//12, 220//12)
        game_state = "start"
        
    display.update()
    

