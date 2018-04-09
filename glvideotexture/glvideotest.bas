'glvideotexture_test a program by NGUYEN.Chung (freeware 2018)
'#Define noguijpeg
'#Define guinogfx
Dim shared As Double auxvar,auxvar2,auxvar3,auxvar4,auxvar5
Dim Shared As Integer quit
#Include Once "windows.bi"
#Include once "gui_chung.bi"
#Include once "gl_chung.bi"
'#Include once "imagesave.bi"
#include Once "win/mmsystem.bi"

Dim Shared As Integer i,j,k,l,n,p,xmax,ymax,winx,winy,file,itime
Dim Shared As String resp
Sub mysubquit
	quit=1
End Sub
Dim Shared As Single scale2=2
Sub subscale2
Dim As Integer i  
getcomboindex("win.scale2",i)
scale2=max(0.2,min(3.0,1+(i-4)*0.2))
Sleep 200
End Sub
Dim Shared As Integer dx,dy
Sub subdx
Dim As Integer i  
getcomboindex("win.dx",i)
dx=i-1
Sleep 200
End Sub
Sub subdy
Dim As Integer i  
getcomboindex("win.dy",i)
dy=i-1
Sleep 200
End Sub
Dim Shared As Integer tinitimage,iimage=1,iimage0=1
Sub subimage
Dim As Integer i  
getcomboindex("win.image",i)
i=max2(0,min2(4,i))
If i<>iimage Then
	iimage0=iimage:iimage=i
	tinitimage=1
EndIf
Sleep 200
End Sub
Dim Shared As Integer tfolderimage
Dim Shared As String folderimage
Sub subfolder()
Dim As String fic,dir0
Dim As Integer ret 
dir0=CurDir
ChDir(ExePath+"\media\")  
fic=filedialog("load","glvideo0.jpg")
fic=Trim(fic)
ret=ChDir(dir0)
folderimage=""
If LCase(Right(fic,12))="glvideo0.jpg" Then
	folderimage=Left(fic,Len(fic)-12)
	'guinotice folderimage
	tfolderimage=1
EndIf 	
End Sub 
Sub subleftmouse
guisetfocus("win.graph2")
Sleep 200
End Sub
Sub initsounds()
	Dim As String soundfic
   soundfic="sounds/hello how are you.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias hello",0,0,0)
   mcisendstring("play hello from 0",0,0,0)
End Sub
Sub closesounds()
   mcisendstring("close hello",0,0,0)
End Sub
initsounds()

Dim Shared As Integer wx,wy,depth
ScreenInfo wx,wy,depth

xmax=wx-20:ymax=wy-30-256+172
guibackgroundcolor(0,0,100)
guistatictextinkcolor(200,200,200)
button("win.quit","quit",@mysubquit,10,10,50,20)
'button("win.pause","pause",@subpause,70,10,50,20)
statictext("win.msg","",72,12,300,20)
combobox("win.scale2",@subscale2,400,10,80,400)
combobox("win.image",@subimage,495,10,80,400)
combobox("win.dx",@subdx,600,10,60,500)
combobox("win.dy",@subdy,670,10,60,500)
button("win.folder","folder image",@subfolder,745,10,100,23)
graphicbox("win.graph2",2,35,xmax,ymax,"opengl")
openwindow("win","glvideotest",4,4,xmax+10,70+ymax)

trapclose("win",@mysubquit)
trapLeftmouse("win.graph2",@subleftmouse)

addcombo("win.scale2","scale0.4")
addcombo("win.scale2","scale0.6")
addcombo("win.scale2","scale0.8")
addcombo("win.scale2","scale1")
addcombo("win.scale2","scale1.2")
addcombo("win.scale2","scale1.4")
addcombo("win.scale2","scale1.6")
addcombo("win.scale2","scale1.8")
addcombo("win.scale2","scale2")
addcombo("win.scale2","scale2.2")
addcombo("win.scale2","scale2.4")
addcombo("win.scale2","scale2.6")
addcombo("win.scale2","scale2.8")
addcombo("win.scale2","scale3")
i=9
selectcomboindex("win.scale2",i)
subscale2()
'scale2=max(0.2,min(3.0,1+(i-4)*0.2))

addcombo("win.image","image")
addcombo("win.image","image2")
addcombo("win.image","image3")
addcombo("win.image","image4")
iimage=1
i=iimage
selectcomboindex("win.image",i)
subimage()

For i=1 To 31
	addcombo("win.dx","dx"+Str(i-1))
Next
selectcomboindex("win.dx",1)
subdx()

For i=1 To 31
	addcombo("win.dy","dy"+Str(i-1))
Next
selectcomboindex("win.dy",1)
subdy()

'setforegroundwindow(getguih("win"))
guisetfocus("win.quit")
Dim Shared As hwnd winh,winmsg
winh=getguih("win")
winmsg=getguih("win.msg")

Declare Sub initgl
Declare Sub gldrawtext0(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
Declare Sub display()
guistartOpenGL("win.graph2")
        initgl
        glclearcolor 0,0.7,0, 0.0
        glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
        i=xmax*0.35:j=ymax*0.4
        glcolor3f(0,0,0.59)
        gldisable gl_lighting
        gldrawtext0("glvideotexture_chung",i,j,3)
        glcolor3f(1,1,1)
        gldrawtext0("a program by NGUYEN.Chung",xmax*0.375,j+70,1.5)
        guirefreshopenGL() 

'Declare Sub testopengl()
'testopengl()
Dim Shared As uint mygltext(11)
Sub inittextures(ii As Integer=1)
Dim As Integer i=0
Var nimage=""
If ii>1 Then nimage=Str(ii)
Var fic=(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
If ii=0 Then
	fic=(folderimage+"glvideo"+Str(i)+".jpg")
   If FileExists(fic)=0 Then
	   guinotice fic+" not found !"
	   Exit Sub
   EndIf 
   For i=0 To 11
	  If mygltext(i)<>0 Then guideletetexture(mygltext(i))
	  mygltext(i)=guiloadtexture(folderimage+"glvideo"+Str(i)+".jpg")
     guiscan
     printgui("win.msg","load texture "+Str(i))
     Sleep 100    
   Next i
   printgui("win.msg",Space(200))
   Exit Sub 
printgui("win.msg",Space(200))
EndIf
If FileExists(fic)=0 Then
	guinotice fic+" not found !"
	iimage=iimage0
	selectcomboindex("win.image",iimage)
   Exit Sub 
EndIf
iimage=ii
iimage0=iimage
For i=0 To 11
	If mygltext(i)<>0 Then guideletetexture(mygltext(i))
	mygltext(i)=guiloadtexture(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
   guiscan
   printgui("win.msg","load texture "+Str(i))
   Sleep 100
Next
printgui("win.msg",Space(200))
End Sub
inittextures(iimage)

Dim Shared As double time0,time1,time2,dtime=0,fps=1,timemsg

While quit=0 And guitestkey(vk_escape)=0
	guiscan
	Sleep 15
   
   If tinitimage=1 Then
   	tinitimage=0
   	inittextures(iimage)
   EndIf
   If tfolderimage=1 Then
   	tfolderimage=0
   	inittextures(0)
   EndIf
   
	time2=time1
	time1=Timer
	fps+=(1.0/max(0.001,time1-time2)-fps)*0.2
	If Timer>timemsg+0.2 Then
		timemsg=Timer 
		printguih(winmsg,"fps "+Str(Int(fps))+"   ")
	EndIf

   display()
	guirefreshopenGL()
 
	If Abs(auxvar)>0.00001 Then
	   printguih(winmsg,"aux="+Str(auxvar)+"          ")
	EndIf
	If guitestkey(vk_escape) Or guitestkey(vk_space) Then
		If winh=getactivewindow() Then quit=1
	EndIf
	
Wend

quit=1
Sleep 1000
guicloseOpenGL()
closesounds()

guiclosewindow("win")
Sleep 1000

guiclose()
guiquit()

End 
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
Sub setvideotexture(ByVal dtime As Double,ByRef itexture As Integer,ByRef tx As Single,ByRef ty As Single, _  
	                 ByRef dtx As Single,ByRef dty As Single,ByVal dx As Integer=0,ByVal dy As Integer=0)
	itexture=Int(dtime)Mod 12
	'glbindtexture gl_texture_2D,mygltext(itexture)
	Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	var videodx=4*Int(512\4)
	Var videody=4*Int(256\4)		
	tx=(ix)/2+dx/videodx
	ty=(3-iy)/4+dy/videody
	dtx=1/2-dx*2/videodx
	dty=1/4-dy*2/videody         
End Sub
Sub display()
Dim As Integer i,j,k 

	glClearColor 0,0.5,1, 1.0
	glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
	
	glloadidentity
		 
	dtime=time1-time0
	Dim As integer itexture=0
	Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
	setvideotexture(dtime,itexture,tx,ty,dtx,dty,dx,dy)
	glenable gl_texture_2D
	glbindtexture gl_texture_2D,mygltext(itexture)
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_linear)
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_linear)'NEAREST)'nomipmap
	glpushmatrix
   glTranslatef (0,0,-70)
   glrotatef(Timer*4,0,1,0)
	Var dx=17.9
	Var sc=scale2
	glscalef(sc,1,sc)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(-dx,-dx,0)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dx,-dx,0)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dx,dx,0)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(-dx,dx,0)
	glend()
	glpopmatrix
	
End Sub

     