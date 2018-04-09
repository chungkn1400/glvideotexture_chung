'glvideotexture a program by NGUYEN.Chung (freeware 2018)
'#Define noguijpeg
'#Define guinogfx
Dim shared As Double auxvar,auxvar2,auxvar3,auxvar4,auxvar5
Dim Shared As Integer quit
#Include Once "windows.bi"
#Include once "gui_chung.bi"
#Include once "gl_chung.bi"
#Include once "imagesave.bi"
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
Dim Shared As Integer size=1024
Sub subsize
Dim As Integer i  
getcomboindex("win.size",i)
size=1024
If i=1 Then size=512
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

Dim Shared As Integer videolen=1,videopos,videodx,videody,tinitimage(100)
Dim Shared As Integer wx,wy,depth
ScreenInfo wx,wy,depth

videodx=4*int(512\4):videody=4*Int(256\4)

size=1024 
guiconfirm("size 1024 ? no=>512","confirm",resp)
If resp="no" Then
	size=512':guinotice "size=512"
   videodx=2*int(512\4):videody=2*Int(256\4)		
   guiconfirm("size 512 ? no=>256","confirm",resp)
   If resp="no" Then
	   size=256:guinotice "size=256"
      videodx=int(512\4):videody=Int(256\4)
   EndIf    		
EndIf

xmax=wx-20:ymax=wy-30-videody+172
Dim Shared As Any Ptr myimage(100),videobuffer
For i=0 To 100
	myimage(i)=ImageCreate(videodx,videody,RGBA(127,127,127,127),32)
Next
guibackgroundcolor(0,0,100)
guistatictextinkcolor(200,200,200)
button("win.quit","quit",@mysubquit,10,10,50,20)
'button("win.pause","pause",@subpause,70,10,50,20)
statictext("win.msg","",72,12,300,20)
combobox("win.scale2",@subscale2,400,10,80,400)
combobox("win.dx",@subdx,500,10,60,500)
combobox("win.dy",@subdy,570,10,60,500)
graphicbox("win.graph",2,35,videodx,videody)
graphicbox("win.graph2",2,35,xmax,ymax,"opengl")
openwindow("win","glvideotexture",4,4,xmax+10,70+ymax)

trapclose("win",@mysubquit)

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

'addcombo("win.size","512")
'addcombo("win.size","1024")
'i=2
'selectcomboindex("win.size",i)
'subsize()


setforegroundwindow(getguih("win"))
guisetfocus("win.quit")
Dim Shared As hwnd winh,winmsg
winh=getguih("win")
winmsg=getguih("win.msg")
Declare Sub initgl
Declare Sub gldrawtext0(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
guistartOpenGL("win.graph2")
        initgl
        glclearcolor 0,0.7,0, 0.0
        glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT)
        i=xmax*0.4:j=ymax*0.4
        glcolor3f(0,0,0.59)
        gldisable gl_lighting
        gldrawtext0("glvideotexture_chung",i,j,3)
        glcolor3f(1,1,1)
        gldrawtext0("a program by NGUYEN.Chung",xmax*0.375,j+70,1.5)
        guirefreshopenGL() 
Declare Sub playvideo()
Declare Sub testopengl()


videobuffer=getguigfxbuffer("win.graph")
Var hwind=getguih("win.graph")
Var hDC= Cast(hdc,GetWindowDC(Cast(hwnd,hwind)))
Var bmpx=videodx,bmpy=videody
bmpx=4*Int(bmpx\4):bmpy=4*Int(bmpy\4)
Var hbmp=guicreateDIBbmp(bmpx,-bmpy)
Var bmpbits=guigetbmpdibbits(hbmp)
Var bmpdc=createcompatibleDC(guiwindc0)
selectobject(bmpdc,hbmp)

guiconfirm("test opengl only ?","confirm",resp)
If resp="yes" Then
	testopengl():quit=1
EndIf

If quit=0 Then
 playvideo()
 mciSendString("play myvid from 0 repeat", 0, 0, 0)	
 videopos=-1
EndIf  

While MultiKey(fb.SC_ESCAPE)=0 And quit=0
	guiscan
	itime+=1:If itime>10000 Then itime=0
   resp=Space(256)+Chr(0)
   mciSendString("status myvid position", resp, 256, 0)
   Var ipos=Int(Val(resp)*99/videolen)
   ipos=max2(0,min2(98,ipos))
   If videopos<>ipos Then
   	videopos=ipos
   	If tinitimage(ipos)=0 Then'And ipos=1 Then
   		tinitimage(ipos)=1
   		mciSendString("pause myvid ", 0, 0, 0)
         bitblt(bmpdc,0,0,bmpx,bmpy,hdc,0,0,srccopy)
         memcpy(myimage(ipos)+32,bmpbits,(bmpx*bmpy-1)*4-32)
   		If ipos<99 Then mciSendString("resume myvid ", 0, 0, 0)
      	printguih(winmsg,"ipos="+Str(ipos)+"   ")
   	EndIf
   EndIf
	If Abs(auxvar)>0.00001 Then
		printguih winmsg,"aux="+Str(auxvar)+"     "
	EndIf
	If guitestkey(vk_escape) Or guitestkey(vk_space) Then
		If winh=getactivewindow() Then quit=1
	EndIf
   If ipos>=98 Then Exit While 
Wend

If quit=0 Then 
 guiconfirm("save images as jpg's ?","confirm",resp)
 If resp="yes" Then
	'i=98
	'ImageSave(myimage(i), ExePath+"/media/image/glvideo"+Str(i)+".jpg")
	Var myimage9=ImageCreate(videodx*2,videody*4,RGBA(127,127,127,127),32)
	For i=0 To 11
		For j=0 To 1
			For k=0 To 3
				Put myimage9,(j*videodx,k*videody),myimage(i*8+j*4+k),PSet
			Next
		Next
		If i<=11 Then	
      	ImageSave(myimage9, ExePath+"/media/image/glvideo"+Str(i)+".jpg")
		EndIf
	Next
	ImageDestroy(myimage9)
 EndIf 

 guiconfirm("test opengl ?","confirm",resp)
 If resp="yes" Then
	testopengl()
 EndIf

EndIf 

quit=1
Sleep 1000
guicloseOpenGL()

ReleaseDC(Cast(hwnd,hWind),Cast(hdc,hDC))
If bmpdc<>0 Then deletedc(bmpdc):bmpdc=0
If hbmp<>0 Then deleteobject(hbmp):hbmp=0

For i=0 To 100
	If myimage(i)<>0 Then ImageDestroy(myimage(i))
Next
mciSendString("close myvid", 0, 0, 0)
guiclosewindow("win")
closesounds()
Sleep 1000

guiclose()
guiquit()

End 
Sub playvideo()
Var fic=exepath+"/media/myvideo.avi"	
'Var fic=exepath+"/media/water.wmv"	

 Var curdir0=CurDir
 ChDir ExePath+"\media\" 
 Dim As String aux 
 aux=filedialog("select video","*.avi,avi files ;*.wmv,wmv files ")
 ChDir curdir0
 If FileExists(aux) Then fic=aux 

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
Dim Shared As uint mygltext(11)
Sub setvideotexture(ByVal dtime As Double,ByRef itexture As Integer,ByRef tx As Single,ByRef ty As Single, _  
	                 ByRef dtx As Single,ByRef dty As Single,ByVal dx As Integer=0,ByVal dy As Integer=0)
	itexture=Int(dtime)Mod 12
	'glbindtexture gl_texture_2D,mygltext(itexture)
	Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	tx=(ix)/2+dx/videodx
	ty=(3-iy)/4+dy/videody
	dtx=1/2-dx*2/videodx
	dty=1/4-dy*2/videody         
End Sub
Sub testopengl
Dim As Integer i,j,k 
For i=0 To 11
	If mygltext(i)<>0 Then guideletetexture(mygltext(i))
	mygltext(i)=guiloadtexture(ExePath+"/media/image/glvideo"+Str(i)+".jpg")
   guiscan
   printgui("win.msg","load texture "+Str(i))
   Sleep 100
Next

hidegui("win.graph")
printgui("win.msg",Space(200))

Dim As double time0=Timer,time1=Timer,time2=Timer,dtime=0,fps=1,timemsg=Timer

While quit=0 And guitestkey(vk_escape)=0
	guiscan
	Sleep 15


	glClearColor 0,0.5,1, 1.0
	glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
	
	glloadidentity
	
	time2=time1
	time1=Timer
	fps+=(1.0/max(0.001,time1-time2)-fps)*0.2
	If Timer>timemsg+0.2 Then
		timemsg=Timer 
		printguih(winmsg,"fps "+Str(Int(fps))+"   ")
	EndIf
	 
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
	
	guirefreshopenGL()

	If Abs(auxvar)>0.00001 Then
	   printguih(winmsg,"aux="+Str(auxvar)+"          ")
	EndIf
	If guitestkey(vk_escape) Or guitestkey(vk_space) Then
		If winh=getactivewindow() Then quit=1
	EndIf
	
Wend
End Sub

