

#include <X11/Xlib.h>
#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
// draw a grid

#define NIL (0)
#define ARRSIZE 6000
static int whiteColor;
static int blackColor;

static int arr_x[ARRSIZE];
static int arr_y[ARRSIZE];
static int lim_n = 0;


void clear_snake(Display *dpy,Window w, GC gc){
  // read 3 values last value becomes the head of the snake
  {
    XSetForeground(dpy,gc,whiteColor);
    int i ;
    for (i = 0 ; i <= lim_n ; i ++ ){
      int dx = 20 + 11 * arr_x[i];
      int dy = 20 + 11 * arr_y[i];	  
      XFillRectangle(dpy, w, gc, dx+1 , dy+1 , 9 ,9);
    }
    XSetForeground(dpy,gc,blackColor);
  }
}


void draw_snake(Display *dpy,Window w, GC gc){  
    // read 3 values last value becomes the head of the snake
    
      int i ;
      for (i = 0 ; i <= lim_n ; i ++ ){
	int dx = 20 + 11 * arr_x[i];
	int dy = 20 + 11 * arr_y[i];	  
	XFillRectangle(dpy, w, gc, dx + 1 , dy + 1, 9 ,9);
      }	
          
}


void draw_grid(Display *dpy,Window w, GC gc){
  {
	int i,j ; // 20 , 1770 for y	
	for (j = 20; j < 480; j = j + 11){
	  XDrawLine(dpy,w,gc, 20 ,j ,1770 ,j );
	  if ( ((j - 20)% 10) == 0 ){
	    XFillRectangle(dpy, w, gc, 10, j, 11, 11);
	  }
	}
	// y fixed 20 , 471
	for (i = 20; i < 1770; i = i + 11){
	  XDrawLine(dpy,w,gc, i , 20 , i , 471);
	  if ( ((i - 20)% 10) == 0 ){
	    XFillRectangle(dpy, w, gc, i,10, 11, 11);
	  }
	}
      }

}

int main(){
  
  {
    int i ;
    for (i = 0 ; i< ARRSIZE; i ++){
     arr_x[i] = 0;
     arr_y[i] = 0;
    }
    lim_n = 0;
  }
  
  Display *dpy = XOpenDisplay(NIL);
  assert(dpy);

  blackColor = BlackPixel(dpy,DefaultScreen(dpy));
  whiteColor = WhitePixel(dpy,DefaultScreen(dpy));
  int width = 1800;
  int height = 500;
  Window w = XCreateSimpleWindow(dpy , DefaultRootWindow(dpy), 0 ,0 ,
				 width , height , 0 , whiteColor ,
				 whiteColor);

  XSelectInput(dpy, w , StructureNotifyMask | KeyPressMask |
	       KeyReleaseMask | KeymapStateMask | ExposureMask | 
	       ButtonPressMask);
  XMapWindow(dpy,w);

  
  // create grahpics context
  GC gc = XCreateGC(dpy,w,0,NIL);

  // tell gc we draw using white
  XSetForeground(dpy,gc,blackColor);
    
  // wait for  map notify event
  while(1){

   
    
    XEvent ev;
    XNextEvent(dpy,&ev);
    switch(ev.type){
    case KeymapNotify:
      XRefreshKeyboardMapping(&ev.xmapping);
      break;

    case Expose:
      //printf("expose . window damaged ...\n");
      break;
      
    case MapNotify:

      /*
      XColor xcolour;
      // I guess XParseColor will work here
      xcolour.red = 32000; xcolour.green = 65000; xcolour.blue = 32000;
      xcolour.flags = DoRed | DoGreen | DoBlue;
      XAllocColor(dpy, cmap, &xcolour);
      XSetForeground(dpy, gc, xcolour.pixel);
      // fill rectangle 100 x 100
      //XFillRectangle(dpy, w, gc, 0, 0, 100, 100);
      */
      
      /* XDrawLine(dpy,w,gc,10,60,180,20); */
      /* XDrawRectangle(dpy,w,gc,10,60,180,20);     */
      /* XDrawPoint(dpy,w,gc,100,200); */
      draw_grid(dpy,w,gc);
      draw_snake(dpy,w,gc);
      
      XFlush(dpy);
      break;
        
    case KeyPress :
      char string[25];
      int len;
      KeySym keysym;
      len = XLookupString(&ev.xkey , string ,25, &keysym, NULL);
      if (len < 0 ){
	if (string[0] == 'r'){
	  string[0] = '\n';
	}
	fputs(string,stdout);
	fflush(NULL);
      }
      else {
	printf("user pressed key [%s]\n",string);      	
      }
      
      break;
      
    case ConfigureNotify:
      if (width != ev.xconfigure.width ||
	  height != ev.xconfigure.height){
	width = ev.xconfigure.width;
	height = ev.xconfigure.height;
	//printf("Size changed to : %d by %d\n", width ,height);
      }
      break;
      
    case ButtonPress:
      XCloseDisplay(dpy);
      return 0;
      
    }// switch return type

    usleep(500);

    //XClearArea (dpy, w, 0, 0, 1750,460, 1);
    XClearArea (dpy, w, 0, 0, 50,50, 1);
      draw_grid(dpy,w,gc);
      clear_snake(dpy,w,gc);      
      {
    int a , b , c ;
    scanf("%d",&a);
    scanf("%d",&b);
    scanf("%d",&c);

    arr_x[a] = b;
    arr_y[a] = c;
    lim_n = a;
    //printf("received %d %d %d \n",a,b,c);
      }
      draw_snake(dpy,w,gc);
      
      XFlush(dpy);


    

  }//while 1

  sleep(5);
  return 0;

}



