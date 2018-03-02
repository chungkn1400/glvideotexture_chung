'glvideotexture a program by NGUYEN.Chung (freeware 2018)
'#Define noguijpeg
'#Define guinogfx
Dim shared As Double auxvar,auxvar2,auxvar3,auxvar4,auxvar5
Dim Shared As Integer quit
'#Include Once "windows.bi"
#Include once "gui_chung.bi"
#include Once "win/mmsystem.bi"
#Include once "gl_chung.bi"

Dim Shared As Integer i,j,k,l,n,p,xmax,ymax,winx,winy,file,itime
Dim Shared As String resp
Sub mysubquit
	quit=1
End Sub
Sub subresize
Dim As Integer x,y,dx,dy
guigetwindowpos("win",x,y,dx,dy,0)
guimovecontrol("win.graph",2,35,dx-400+390,dy-170+104)
Sleep 100
mciSendString("put myvid destination", 0, 0, 0)
End Sub
Randomize()

Dim Shared As Integer videolen=1,videopos
Dim Shared As Integer wx,wy,depth
ScreenInfo wx,wy,depth

xmax=wx-20:ymax=wy-230
Dim Shared As Any Ptr myimage,videobuffer
myimage=ImageCreate(xmax,ymax,,32)
guibackgroundcolor(0,0,100)
guistatictextinkcolor(200,200,200)
button("win.quit","quit",@mysubquit,10,10,50,20)
'button("win.pause","pause",@subpause,70,10,50,20)
statictext("win.msg","",12,40,300,20)
graphicbox("win.graph",2,35,390,104)
graphicbox("win.graph2",2,35+110,xmax,ymax)
openwindow("win","glvideotexture",10,10,xmax+10,70+112+ymax,WS_MINIMIZEBOX+WS_MAXIMIZEBOX+WS_SYSMENU)
trapclose("win",@mysubquit)
setforegroundwindow(getguih("win"))
guisetfocus("win.quit")
Var winh=getguih("win")
hidegui("win.msg")
trapresize("win",@subresize)
Declare Sub initgl
Declare Sub gldrawtext0(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
guistartOpenGL("win.graph2")
        initgl
        glclearcolor 0,0.7,0, 0.0
        glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
        i=xmax*0.39:j=ymax*0.4
        glcolor3f(0,0,0.59)
        gldisable gl_lighting
        gldrawtext0("glvideotexture_chung",i,j,3)
        glcolor3f(1,1,1)
        gldrawtext0("a program by NGUYEN.Chung",xmax*0.375,j+70,1.5)
        guirefreshopenGL() 
Declare Sub playvideo()
videobuffer=getguigfxbuffer("win.graph")
playvideo()
mciSendString("play myvid from 0 repeat", 0, 0, 0)	

While MultiKey(fb.SC_ESCAPE)=0 and quit=0
	Sleep 30
	guiscan
	itime+=1:If itime>10000 Then itime=0
   resp=Space(256)+Chr(0)
   mciSendString("status myvid position", resp, 256, 0)
   Var ipos=Int(Val(resp)*100/videolen)
   auxvar=ipos
   If videopos<>ipos Then
   	Put myimage,(0,0),videobuffer,PSet
   EndIf    
   'mcisendstring("seek myvid to "+Str(position),0,0,0)
   'mcisendstring("resume myvid",0,0,0) 
	If Abs(auxvar)>0.00001 Then
		showgui("win.msg"):printgui("win.msg"),"aux="+Str(auxvar)+"     "
	EndIf
	If guitestkey(vk_escape) Or guitestkey(vk_space) Then
		If winh=getactivewindow() Then quit=1
	EndIf
Wend

quit=1
Sleep 1000
guicloseOpenGL()

ImageDestroy(myimage)
mciSendString("close myvid", 0, 0, 0)
guiclosewindow("win")
Sleep 1000

guiclose()
guiquit()

End 
Sub playvideo()
Var fic=exepath+"/media/seashore.avi"	
mciSendString("open """ & fic & """ type MPEGVideo Alias myvid", 0, 0, 0)
mciSendString("window myvid handle " +str(getguih("win.graph")), 0, 0, 0)
mciSendString("put myvid destination", 0, 0, 0)
mciSendString("setaudio myvid volume to 1", 0, 0, 0)
mcisendstring("set myvid time format ms",0,0,0)
mcisendstring("set myvid seek exactly on",0,0,0)
Var resp=Space(257)+Chr(0)
mciSendString("status myvid length", resp, 256, 0)
videolen=max2(1,Val(resp))
'guinotice resp 
'mciSendString("play myvid from 0 repeat", 0, 0, 0)	
'mcisendstring("pause myvid",0,0,0) 
End Sub
Sub initgl
        /' init OpenGL '/        
	glMatrixMode GL_PROJECTION
	glLoadIdentity
	'              anglevue    xmax/ymax   mxmin,mxmax
	'gluPerspective   48.0,    640.0/480.0,  2.0, 13000.0
	'gldistmax=13000*10
	'gluPerspective   48.0,    xmax/ymax,  2.0, gldistmax*25
	gluPerspective   48.0,    xmax/ymax,  5.3, 100000'1700000
	'gluPerspective   89.0,    xmax/ymax,  3.3, 100000'1700000
	glMatrixMode GL_MODELVIEW
	glLoadIdentity
	
	glShadeModel GL_SMOOTH
	glClearColor 0.50, 0.50, 1.0, 0.0
	glClearDepth 1.0
	glEnable GL_DEPTH_TEST
	glDepthFunc GL_LEQUAL
	
	'gldisable gl_cull_face
	glenable gl_lighting
	glenable gl_light0
	glenable gl_normalize
	'glnormals=1
   glEnable GL_COLOR_MATERIAL
   glColorMaterial GL_FRONT_and_back,GL_DIFFUSE

	glHint GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST    '' Really Nice Perspective Calculations
	'glHint GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST    '' Really Nice Perspective Calculations
	
	'glenable gl_polygon_smooth
	'glhint gl_polygon_smooth_hint,gl_fastest
	
	'glenable GL_ARB_multitexture
		
	Dim LightAmbient(0 to 3) as single => {0.625, 0.625, 0.625, 1.0}   '' Ambient Light Values
	Dim LightAmbient2(0 to 3) as single => {0.7,0.7,0.7, 1.0}   '' Ambient Light Values
	Dim LightAmbient3(0 to 3) as single => {0.1,0.1,0.1, 1.0}   '' Ambient Light Values
	Dim LightDiffuse(0 to 3) as single => {1.0, 1.0, 1.0, 1.0}   '' Diffuse Light Values
	Dim LightPosition(0 to 3) as single => {0.0, 0.0, 2000.0, 1.0}  '' Light Position
   gllightfv(gl_light0,GL_ambient,glparam4f(1,1,1,0)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
	'glLightfv GL_LIGHT1, GL_AMBIENT, @LightAmbient(0)   '' Setup The Ambient Light
	'glLightfv GL_LIGHT2, GL_AMBIENT, @LightAmbient2(0)   '' Setup The Ambient Light
	'glLightfv GL_LIGHT3, GL_AMBIENT, @LightAmbient3(0)   '' Setup The Ambient Light
	'glLightfv GL_LIGHT0, GL_DIFFUSE, @LightDiffuse(0)   '' Setup The Diffuse Light
	'glLightfv GL_LIGHT0, GL_POSITION, @LightPosition(0) '' Position The Light
	'glEnable GL_LIGHT0                                  '' Enable Light One
 
End Sub
Sub gldrawtext0(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
   glLoadIdentity()
   gldisable(gl_texture_2d)
   'glTranslatef (22*(-1+2*x/xmax)*(xmax/ymax)*(520/700),17*(1-2*y/ymax),-38)
   glTranslatef ((-0.31+23.7*(-1+2*x/xmax)*(xmax/ymax)*(520/700)),-0.25+17.1*(1-2*y/ymax),-40)
   glscalef(scale,scale,scale)
   guigltext(text)
   glenable(gl_texture_2d)
End Sub
Sub gldrawtext(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
   glLoadIdentity()
   glenable(gl_texture_2d)
   glbindtexture(gl_texture_2d,myguifont256text)
   'glTranslatef (22*(-1+2*x/xmax)*(xmax/ymax)*(520/700),17*(1-2*y/ymax),-38)
   glTranslatef ((-0.31+23.7*(-1+2*x/xmax)*(xmax/ymax)*(520/700)),-0.25+17.1*(1-2*y/ymax),-40)
   glscalef(scale*1.1,scale,scale)
   'glscalef(scale,scale,scale)
   myguigltext(text)
End Sub
