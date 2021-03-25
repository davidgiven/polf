/*
Copyright (c) 2004-2019, Lode Vandevenne

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <cmath>
#include <string>
#include <vector>
#include <iostream>
#include <unistd.h>
#include <algorithm>
#include "Fixed.h"

#include "quickcg.h"
using namespace QuickCG;

/*
g++ *.cpp -lSDL -O3 -W -Wall -ansi -pedantic
g++ *.cpp -lSDL
*/

//place the example code below here:

#define screenWidth 80
#define screenHeight 50
#define mapWidth 16
#define mapHeight 16

int worldMap[mapWidth][mapHeight]=
{
  {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,3,0,0,0,0,3,0,0,0,0,0,0,1},
  {1,0,0,0,0,3,0,0,0,0,0,3,0,0,0,1},
  {1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,1},
  {1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,1},
  {1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,1},
  {1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,1},
  {1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
  {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
};

typedef numeric::Fixed<12,4> number_t;

number_t abs(number_t n)
{
    return (number_t) abs(n.to_double());
}

int main(int /*argc*/, char */*argv*/[])
{
  number_t posX = 8, posY = 12;  //x and y start position
  uint8_t dir = 0;

  double time = 0; //time of current frame
  double oldTime = 0; //time of previous frame

  auto torad = [](uint8_t bgrad) { return (double)bgrad * (M_PI*2/256); };

  int8_t sincos_table[256+64];
  for (int i=0; i<256+64; i++)
    sincos_table[i] = 127.0*sin(torad(i));

  auto tsin = [&](uint8_t t) { return sincos_table[t] / 128.0; };
  auto tcos = [&](uint8_t t) { return sincos_table[t+0x40] / 128.0; };

  int8_t deltadistx_table[256];
  int8_t deltadisty_table[256];
  for (int i=0; i<256; i++)
  {
      double deltaDistX = abs(1 / sin(torad(i)));
      deltadistx_table[i] = std::clamp(deltaDistX, -7.9, 7.9) * 16.0;
      double deltaDistY = abs(1 / cos(torad(i)));
      deltadisty_table[i] = std::clamp(deltaDistY, -7.9, 7.9) * 16.0;
  }

  screen(screenWidth, screenHeight, 0, "Raycaster");
  while(!done())
  {
    usleep(10*1000);

    number_t dirX = tsin(dir);
    number_t dirY = tcos(dir);

    for(int x = 0; x < w; x++)
    {
      //calculate ray position and direction
      uint8_t q = dir - 40 + x;
      number_t rayDirX = tsin(q);
      number_t rayDirY = tcos(q);
      //which box of the map we're in
      int mapX = int(posX);
      int mapY = int(posY);

      //length of ray from current position to next x or y-side
      number_t sideDistX;
      number_t sideDistY;

       //length of ray from one x or y-side to next x or y-side
      number_t deltaDistX = deltadistx_table[q] / 16.0;
      number_t deltaDistY = deltadisty_table[q] / 16.0;
      number_t perpWallDist;

      //what direction to step in x or y-direction (either +1 or -1)
      int stepX;
      int stepY;

      int hit = 0; //was there a wall hit?
      int side; //was a NS or a EW wall hit?
      //calculate step and initial sideDist
      if(rayDirX < 0)
      {
        stepX = -1;
        sideDistX = (posX - mapX) * deltaDistX;
      }
      else
      {
        stepX = 1;
        sideDistX = (mapX + 1.0 - posX) * deltaDistX;
      }
      if(rayDirY < 0)
      {
        stepY = -1;
        sideDistY = (posY - mapY) * deltaDistY;
      }
      else
      {
        stepY = 1;
        sideDistY = ((number_t)mapY + 1.0 - posY) * deltaDistY;
      }
      //perform DDA

	  printf("%d ", q);
      while (hit == 0)
      {
        //jump to next map square, OR in x-direction, OR in y-direction
        if(sideDistX < sideDistY)
        {
          sideDistX += deltaDistX;
          mapX += stepX;
          side = 0;
        }
		else
        {
          sideDistY += deltaDistY;
          mapY += stepY;
          side = 1;
        }

        //Check if ray has hit a wall
        if (worldMap[mapX][mapY])
			hit = 1;
      }
      //Calculate distance projected on camera direction (Euclidean distance will give fisheye effect!)
	  if (side == 0)
          perpWallDist = (mapX - posX + (1 - stepX) / 2) / rayDirX;
	  else
          perpWallDist = (mapY - posY + (1 - stepY) / 2) / rayDirY;

      //Calculate height of line to draw on screen
      int lineHeight = (perpWallDist > 0.01) ? (int)((number_t)h / perpWallDist) : 1;

      //calculate lowest and highest pixel to fill in current stripe
      int drawStart = -lineHeight / 2 + h / 2;
      if(drawStart < 0)drawStart = 0;
      int drawEnd = lineHeight / 2 + h / 2;
      if(drawEnd >= h)drawEnd = h - 1;

      //choose wall color
      ColorRGB color;
      switch(worldMap[mapX][mapY])
      {
        case 1:  color = RGB_Red;    break; //red
        case 2:  color = RGB_Green;  break; //green
        case 3:  color = RGB_Blue;   break; //blue
        case 4:  color = RGB_White;  break; //white
        default: color = RGB_Yellow; break; //yellow
      }

      //give x and y sides different brightness
      if(side == 1) {color = color / 2;}

      //draw the pixels of the stripe as a vertical line
      if (drawEnd > drawStart)
      {
          verLine(x, drawStart, drawEnd, color);
      }
    }
    //timing for input and FPS counter
    oldTime = time;
    time = getTicks();
    number_t frameTime = (time - oldTime) / 1000.0; //frameTime is the time this frame has taken, in seconds
    redraw();
    cls();

    //speed modifiers
    number_t moveSpeed = frameTime * 5.0; //the constant value is in squares/second
    number_t rotSpeed = frameTime * 3.0; //the constant value is in radians/second
    moveSpeed = 0.2;
    rotSpeed = 1.0;
    readKeys();
    //move forward if no wall in front of you
    if(keyDown(SDLK_UP))
    {
      if(worldMap[int(posX + dirX * moveSpeed)][int(posY)] == false) posX += dirX * moveSpeed;
      if(worldMap[int(posX)][int(posY + dirY * moveSpeed)] == false) posY += dirY * moveSpeed;
    }
    //move backwards if no wall behind you
    if(keyDown(SDLK_DOWN))
    {
      if(worldMap[int(posX - dirX * moveSpeed)][int(posY)] == false) posX -= dirX * moveSpeed;
      if(worldMap[int(posX)][int(posY - dirY * moveSpeed)] == false) posY -= dirY * moveSpeed;
    }
    //rotate to the right
    if(keyDown(SDLK_RIGHT))
    {
      dir++;
    }
    //rotate to the left
    if(keyDown(SDLK_LEFT))
    {
      dir--;
    }
  }
}
