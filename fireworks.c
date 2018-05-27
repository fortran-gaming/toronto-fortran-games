/*
 * Fireworks program from UNIXWORLD magazine, Sept. 1991
 *
 * Author: Ray Swartz, Berkeley Decision/Systems.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <curses.h>
#include <ctype.h>
#include <sys/types.h>
#include <time.h>

#define DELAYSIZE 100000	/* delay counter */

int main()
{
	int start;		/* starting column (bottom row) */
	int end;		/* ending column */
	int row;		/* for loop height counter */
	int diff;		/* lines to ascend = abs(start-end) */
	int flag;		/* 1/0 value to control ascending refresh */
	int direction;		/* -1 = right to left, 1 = left to right */
	void curquit();		/* terminates curses properly on interrupt */
	void myrefresh();	/* refresh() with a delay counter */
	int seed;		/* random number seed */
	void explode();		/* draw the explosion */

	signal(SIGINT, curquit);	/* terminate with interrupt */
	initscr();		/* initialize curses */
	seed = time((time_t *)0);	/* use time in seconds as seed */
	srand(seed);		/* seed random generator */
	while (true) {		/* keep drawing until interrupted */
		do {		/* calculate rocket coordinates */
			start = rand() % (COLS-3);	/* bottom column */
			end = rand() % (COLS-3);	/* top column */
			start = (start < 2) ? 2 : start;	/* watch edges */
			end = (end < 2) ? 2 : end;	/* watch edges */
			/* direction = -1 is right to left, 1 is left to right */
			direction = (start > end) ? -1 : 1;
			diff = abs(start - end);	/* height = width */
		} while (diff<2 || diff>=LINES-2);	/* fit to screen */
		for (row=0; row<diff; row++) {	/* draw rocket going up */
			mvprintw(LINES-row, start+(row*direction),
			 (direction < 0) ? "\\" : "/");
			if (flag++) {	/* refresh every other time */
				myrefresh();
				clear();
				flag=0;
			}
		}
		if (flag++) {	/* refresh if flag == 1 at end of loop */
			myrefresh();
			flag = 0;
		}
		explode(LINES-row, start+(diff*direction));	/* showtime */
		clear();	/* prepare for next rocket */
		myrefresh();
	}
}

void curquit()			/* be sure to quit curses properly */
{
	endwin();		/* required to return terminal to proper mode */
	exit(EXIT_SUCCESS);
}

void explode(row, col)		/* draw explosion */
int row;			/* (row,col) is center of explosion */
int col;
{

  void myrefresh();
  
	clear();
	mvprintw(row  , col  , "-");		/* step 1 */
	myrefresh();
	mvprintw(row-1, col-1, " - ");		/* step 2 */
	mvprintw(row  , col-1, "-+-");
	mvprintw(row+1, col-1, " - ");
	myrefresh();
	mvprintw(row-2, col-2, " --- ");	/* step 3 */
	mvprintw(row-1, col-2, "-+++-");
	mvprintw(row  , col-2, "-+#+-");
	mvprintw(row+1, col-2, "-+++-");
	mvprintw(row+2, col-2, " --- ");
	myrefresh();
	mvprintw(row-2, col-2, " +++ ");	/* step 4 */
	mvprintw(row-1, col-2, "++#++");
	mvprintw(row  , col-2, "+# #+");
	mvprintw(row+1, col-2, "++#++");
	mvprintw(row+2, col-2, " +++ ");
	myrefresh();
	mvprintw(row-2, col-2, "  #  ");	/* step 5 */
	mvprintw(row-1, col-2, "## ##");
	mvprintw(row  , col-2, "#   #");
	mvprintw(row+1, col-2, "## ##");
	mvprintw(row+2, col-2, "  #  ");
	myrefresh();
	mvprintw(row-2, col-2, " # # ");	/* step 6 */
	mvprintw(row-1, col-2, "#   #");
	mvprintw(row  , col-2, "     ");
	mvprintw(row+1, col-2, "#   #");
	mvprintw(row+2, col-2, " # # ");
	myrefresh();
	
}

void myrefresh()
{
	usleep(DELAYSIZE);	/* timer to slow screen refresh down */
	move(LINES-1, COLS-1);	/* keep cursor out of the way */
	refresh();
	/* Uncomment next line if pressing RETURN locks up terminal */
	/* flushinp(); */
}
