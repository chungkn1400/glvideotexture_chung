'seashore a program by NGUYEN.Chung (freeware 2018)
'#Define noguijpeg
'#Define guinogfx
Dim shared As Double auxvar,auxvar2,auxvar3,auxvar4,auxvar5,auxvar6,auxtest=1
Dim Shared As String auxtext
Dim Shared As Integer quit
#Include Once "windows.bi"
#Include once "gui_chung.bi"
#Include once "gl_chung.bi"
'#Include once "imagesave.bi"
#include Once "win/mmsystem.bi"

#Inclib "./load3ds_chung"
Declare Function load3DS Cdecl Alias "load3DS" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr) As Integer        
'call with autoresize desired size=integer 
Declare Function load3DSsize Cdecl Alias "load3DSsize" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr, ByVal size As uint) As Integer        
Declare Function load3DSsizesmooth Cdecl Alias "load3DSsizesmooth" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr, ByVal size As uint) As Integer        

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
Dim Shared As Single heure=12
Dim Shared As Integer tdark=0
Sub subhour
Dim As Integer i  
getcomboindex("win.hour",i)
i=max2(0,min2(24,(i-1)*2))
heure=i
Sleep 200
End Sub
Dim Shared As Double timemouse
Dim Shared As Integer mouseforward,mouseleft,mouseright,mouseback,mousex,mousey,mousedown,mouseup,mouseon
Sub subleftmouse
mouseforward=0:mouseleft=0:mouseright=0:mouseback=0
mouseup=0:mousedown=0
mousex=guimousex
mousey=guimousey
mouseon=1
If mousex<xmax*0.43 Then
	mouseleft=1
ElseIf mousex>xmax*0.57 Then
   mouseright=1
ElseIf mousey<ymax*0.35 Then
	mouseup=1
ElseIf mousey<ymax*0.65 Then
   mouseforward=1
ElseIf mousey<ymax*0.87 then
   mouseback=1
Else
	mousedown=1
EndIf       
guisetfocus("win.graph2")
Sleep 50
timemouse=Timer 
End Sub
Sub subleftmouseup
mouseforward=0:mouseleft=0:mouseright=0:mouseback=0
mouseup=0:mousedown=0
mousex=guimousex
mousey=guimousey
mouseon=0
guisetfocus("win.graph2")
Sleep 50
End Sub
Sub submovemouse
If mouseon=0 Then Exit Sub 
mouseforward=0:mouseleft=0:mouseright=0:mouseback=0
mouseup=0:mousedown=0
mousex=guimousex
mousey=guimousey
If mousex<xmax*0.43 Then
	mouseleft=1
ElseIf mousex>xmax*0.57 Then
   mouseright=1
ElseIf mousey<ymax*0.35 Then
	mouseup=1
ElseIf mousey<ymax*0.65 Then
   mouseforward=1
ElseIf mousey<ymax*0.87 then
   mouseback=1
Else
	mousedown=1
EndIf       
End Sub
Sub initsounds()
	Dim As String soundfic
   soundfic="sounds/hello how are you.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias hello",0,0,0)
   soundfic="sounds/ocean.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias ocean",0,0,0)
   soundfic="sounds/nature.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias nature",0,0,0)
   mcisendstring("play hello from 0",0,0,0)
   mcisendstring("play ocean from 0 repeat",0,0,0)
   mcisendstring("play nature from 0 repeat",0,0,0)
	mcisendstring("setaudio nature volume to "+Str(Int(10)),0,0,0)
End Sub
Sub closesounds()
   mcisendstring("close hello",0,0,0)
   mcisendstring("close ocean",0,0,0)
   mcisendstring("close nature",0,0,0)
   mcisendstring("close all",0,0,0)
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
'combobox("win.image",@subimage,495,10,80,400)
combobox("win.hour",@subhour,495,10,60,500)
graphicbox("win.graph2",2,35,xmax,ymax,"opengl")
openwindow("win","seashore",4,4,xmax+10,70+ymax)

trapclose("win",@mysubquit)
trapLeftmouse("win.graph2",@subleftmouse)
trapLeftmouseup("win.graph2",@subleftmouseup)
trapmovemouse("win.graph2",@submovemouse)

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

/'
addcombo("win.image","image")
addcombo("win.image","image2")
addcombo("win.image","image3")
addcombo("win.image","image4")
iimage=1
i=iimage
selectcomboindex("win.image",i)
subimage()
'/
For i=0 To 24 Step 2
	addcombo("win.hour",Str(i)+"h")
Next
heure=Val(Left(Time,2))+0.1
selectcomboindex("win.hour",Int(heure/2)+1)
subhour()

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
        gldrawtext0("seashore_chung",i,j,3)
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
'Var fic=(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
Var fic=(ExePath+"/media/seashore/glvideo"+Str(i)+".jpg")
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
	'mygltext(i)=guiloadtexture(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
	mygltext(i)=guiloadtexture(ExePath+"/media/seashore/glvideo"+Str(i)+".jpg")
   guiscan
   printgui("win.msg","load texture "+Str(i))
   Sleep 100
Next
printgui("win.msg",Space(200))
End Sub
inittextures(iimage)

Const As Single degtorad=ASin(1)/90
Const As Single radtodeg=90/ASin(1)
Dim Shared As Single mx,my,mz,yh,dmx,dmy,dmz
Dim Shared As Single o1,o2,o3,cos1=1,sin1,cos2=1,sin2,cos3=1,sin3
Dim Shared As Integer tactive

Dim Shared As double time0,time1,time2,dtime=0,fps=1,timemsg,kfps=1

While quit=0 And guitestkey(vk_escape)=0
	guiscan
	Sleep 10
   
   If tinitimage=1 Then
   	tinitimage=0
   	inittextures(iimage)
   EndIf
   
	time2=time1
	time1=Timer
	fps+=(1.0/max(0.001,time1-time2)-fps)*0.2
	kfps=30.0/max(1.0,fps)
	If Timer>timemsg+0.2 Then
		timemsg=Timer 
		printguih(winmsg,"fps "+Str(Int(fps))+"   ")
		tactive=0
		If winh=getactivewindow() Then tactive=1
		Var vol=max(5.0,min(1000.0,1000*200/max(200.0,330-mx)))
		mcisendstring("setaudio ocean volume to "+Str(Int(vol)),0,0,0)
		vol=max(5.0,min(1000.0,230*100/max(100.0,1100+mx)))
		mcisendstring("setaudio nature volume to "+Str(Int(vol)),0,0,0)
	EndIf

   If tactive=1 Then
    Var vv=1.5
    If guitestkey(vk_numpad1) Then o1-=3*kfps
    If guitestkey(vk_numpad3) Then o1+=3*kfps
    If guitestkey(vk_numpad8) Or guitestkey(vk_b)Or guitestkey(vk_prior)Or mouseup Then o2=min(89.0,o2+3*kfps)
    If guitestkey(vk_numpad2) Or guitestkey(vk_n)Or guitestkey(vk_next)Or mousedown Then o2=max(-89.0,o2-3*kfps)
    If guitestkey(vk_numpad5) Then o2=0
    If guitestkey(vk_left) Or guitestkey(vk_numpad1) Or mouseleft Then o1+=3*kfps
    If guitestkey(vk_right) Or guitestkey(vk_numpad3) Or mouseright Then o1-=3*kfps
    If guitestkey(vk_up) Or mouseforward Then
    	 mx+=vv*cos1*kfps:my+=vv*sin1*kfps
    	 If Abs(o2)<13 Or o2>22 Or o2<-50 Then o2=0
    EndIf
    If guitestkey(vk_down) Or mouseback Then
    	 mx-=0.5*vv*cos1*kfps:my-=0.5*vv*sin1*kfps
    	 If Abs(o2)<13 Or o2>22 Or o2<-50 Then o2=0
    EndIf
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
Function diro1(dx As Single,dy As Single)As Single 
Dim As Single do1
If Abs(dx)>max(0.000001,0.001*Abs(dy)) Then
   If dx>0 Then
   	do1=Atn(dy/dx)*radtodeg
   Else
   	do1=180-Atn(dy/Abs(dx))*radtodeg
   EndIf
Else
	do1=Sgn(dy)*90
EndIf
Return do1
End Function 
Dim Shared As uint sandtext
Sub drawsand
If sandtext=0 Then sandtext=guiloadtexture(ExePath+"/media/sand.jpg")
glbindtexture gl_texture_2D,sandtext
Dim As single dx=-3000,dxx=-2000,dtx=3.5,dty=5,tx=0,ty=0
   Var ddx=2*dx/dtx,ddy=2*0.7*dx/dty
   Var cc=0.65
   glpushmatrix 
   gltranslatef(Int(mx/ddx)*ddx,Int(my/ddy)*ddy,0)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glcolor3f(1,1,1)
	glvertex3f(-dxx,-dx,0)
	gltexcoord2f(tx+dtx,ty)
   glcolor3f(cc,cc,cc)
	glvertex3f(dx,-dx,0)
	glTexCoord2f(tx+dtx,ty+dty)
   glcolor3f(cc,cc,cc)
	glvertex3f(dx,dx,0)
	gltexcoord2f(tx,ty+dty)
	glcolor3f(1,1,1)
	glvertex3f(-dxx,dx,0)
	glend()
	glpopmatrix
	glcolor3f(1,1,1)
End Sub
Sub drawsea0()
Dim As Integer i,j,k 
Var dz0=-50.0,dzz0=-50.0,dzz=-50.0,dz=-50.0	
glenable gl_texture_2D
glbindtexture gl_texture_2D,mygltext(itime)
glnormal3f(0,0,1)
For i=-30 To 30
	glpushmatrix
	Var sc=scale2*1.5
	Var ky=sc*17.8*2*2
	dzz0=dz0
	dz0=-Abs(i*1.9)-90/max(9.0,115-mx)
   glTranslatef (100,ky*(i/2+Int(my/ky-0.5)),0)
   If ((i+200) Mod 2)=1 Then
   	glscalef(-1,-1,1)
   	dzz=dz0
   	dz=dzz0
   Else
   	dzz=-dzz0
   	dz=-dz0
   EndIf
   'glrotatef(90,0,0,1)
   'glrotatef(90,1,0,0)
	Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	Var tx=(ix)/2
	Var ty=(3-iy)/4,dtx=1/2,dty=0.86/4
	Var dx=17.9
	Var dxx=0.0
	'Var sc=scale2*1.6
	glscalef(sc,sc,1)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz,dx,dx+dx-Abs(dz*1.6)-7)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz,-dx,dx+dx-Abs(dzz*1.6)-7)
	glend()
	Var d7=0.3*dx*Sgn(dzz)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx-d7,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx-d7,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*4,dx-d7,dx*1.4)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*4,-dx-d7,dx*1.4)
	glend()
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(dzz*2,-dx,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dz*2,dx,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*6,dx,dx*1.4)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*6,-dx,dx*1.4)
	glend()
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(dzz*6,-dx-d7,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dz*6,dx-d7,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*10,dx-d7,dx*1.5)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*10,-dx-d7,dx*1.5)
	glend()
	glpopmatrix
Next i		
End Sub
Sub drawseaband(x0 As Single,x1 As Single,kdy As Single,kty0 As Single,kty1 As Single)
Dim As Integer i,j,k 
For i=-30 To 30
	glpushmatrix
	Var sc=scale2*1.5
	Var ky=sc*17.8*2*2
   glTranslatef (0,2*ky*(i/2+Int(my/ky-0.5)),0)
   Var dzz=1.0
   If ((i+200) Mod 2)=1 Then
   	glscalef(1,-1,1)
   	dzz=-1.0
   EndIf
   'glrotatef(90,0,0,1)
   'glrotatef(90,1,0,0)
	glenable gl_texture_2D
	glbindtexture gl_texture_2D,mygltext(itime)
	Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	Var tx=(ix)/2
	Var ty=(3-iy)/4,dtx=1/2,dty=0.86/4
	Var dx=17.9
	'Var sc=scale2*1.6
	glscalef(sc,sc*2,1)
	/'glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz,dx,dx+dx-Abs(dz*1.6)-7)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz,-dx,dx+dx-Abs(dzz*1.6)-7)
	glend() '/
	Var d7=kdy*dx*Sgn(dzz)
	Var ty0=ty+kty0*dty
	Var ty1=ty+kty1*dty
	glbegin(gl_quads)
	glTexCoord2f(tx,ty0)
	glvertex3f(x0,-dx-d7,1)
	gltexcoord2f(tx+dtx,ty0)
	glvertex3f(x0,dx-d7,1)
	glTexCoord2f(tx+dtx,ty1)
	glvertex3f(x1,dx-d7,1)
	gltexcoord2f(tx,ty1)
	glvertex3f(x1,-dx-d7,1)
	glend()
	glpopmatrix
Next i		
End Sub
Sub drawsea()
Dim As Integer i,j,k
Var dx=0.0,dxx=0.0,di=10
Var d1000=1000.0
Var ty=0.0,tyy=0.0
Var do11=diro1(d1000-mx,mz+yh-0)
Var do10=max(0.001,diro1(100-mx,mz+yh-0))
Var do1=do10
auxvar=do11
auxvar2=do10
dx=(mz+yh-0)/max(0.001,Tan(do10*degtorad))
For i=1 To di
	do1=do10+(do11-do10)*i/di
	tyy=ty
	ty=(do1-do10)/min(-0.001,do11-do10)	
	dxx=dx
	dx=mx+(mz+yh-0)/max(0.001,Tan(do1*degtorad))
	If i=1 Then auxvar3=dx
	drawseaband(dxx,dx,0,tyy,ty)
Next
End Sub
Declare Sub drawtrees()
Declare Sub drawshadowtrees()
Declare Sub drawshadowrocs()
Declare Sub drawrocs()
Declare Sub drawsunset()
Declare Sub drawskydome(rx As Single,ix0 As Integer,iy0 As Integer)
Sub display()
Dim As Integer i,j,k 

	glClearColor 0.5,0.5,1, 1.0
	glClear GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT
	
	glloadidentity
		 
   yh=28'50'28
   mx=max(-1800.0,min(230.0,mx))

   cos1=Cos(o1*degtorad):sin1=Sin(o1*degtorad)
   cos2=Cos(o2*degtorad):sin2=Sin(o2*degtorad)
   cos3=Cos(o3*degtorad):sin3=Sin(o3*degtorad)
   dmx=cos1*cos2:dmy=sin1*cos2:dmz=sin2
	glulookat(mx,my,mz+yh, mx+1000*dmx,my+1000*dmy,mz+1000*dmz+yh, 0,0,1)

	dtime=time1-time0
	itime=Int(dtime)Mod 12
	
	
	If tdark=1 Then glenable gl_lighting

	glpushmatrix
   gltranslatef(mx,my,0)
   drawskydome(6000,14,14)
   glpopmatrix 	
   drawsunset()

	drawsand()
	
	drawsea0()
	
	drawshadowrocs()
	drawshadowtrees()
	drawtrees()
   
   'glenable gl_lighting
	drawrocs()
   glnormal3f(0,0,1)
	
   gldisable gl_lighting
   
	    gldisable gl_depth_test
	    
       If auxtest>0.01 Then  
        If Abs(auxvar)>0.00001 Then gldrawtext("aux= "+Str(auxvar),15,ymax-79,1.2)
       EndIf
       If auxtest>0.1 Then 
        If Abs(auxvar2)>0.00001 Then gldrawtext("aux2= "+Str(auxvar2),15,ymax-99,1.2)
        If Abs(auxvar3)>0.00001 Then gldrawtext("aux3= "+Str(auxvar3),15,ymax-119,1.2)
        If Abs(auxvar4)>0.00001 Then gldrawtext("aux4= "+Str(auxvar4),15,ymax-139,1.2)
        If Abs(auxvar5)>0.00001 Then gldrawtext("aux5= "+Str(auxvar5),15,ymax-159,1.2)
        If Abs(auxvar6)>0.00001 Then gldrawtext("aux6= "+Str(auxvar6),15,ymax-179,1.2)
        If auxtext<>"" Then gldrawtext(auxtext,15,ymax-199,1.2)
       EndIf
       gldrawtext("x= "+Str(Int(mx))+"  y= "+Str(Int(my))+"  z= "+Str(Int(mz)),15,ymax-10,1.2)

	    glenable gl_depth_test
	    
End Sub
Dim Shared As Single ksoleil,heure0,kxsoleil,kysoleil,kzsoleil,suno1,suntan2=1
Sub setksoleil
  If Abs(heure-heure0)<0.001 Then Exit Sub 
  heure0=heure
  Var aux=1.0	
  If heure<=4 or heure>=21 Then
  	  ksoleil=0.4
  EndIf 
  If heure>4 And heure<=6 Then
  	  aux=(heure-4)/2
  	  ksoleil=0.6074*aux+(1-aux)*0.4
  EndIf     
  If heure>6 And heure<=8 Then
  	  aux=(heure-6)/2
  	  ksoleil=1.0*aux+(1-aux)*0.6'0.74
  EndIf
  If heure>8 And heure<=17 Then
  	  ksoleil=1.0
  EndIf
  If heure>17 And heure<=19 Then 
  	  aux=(19-heure)/2
  	  ksoleil=1.0*aux+(1-aux)*0.6'0.74 
  EndIf
  If heure>19 And heure<=21 Then 
  	  aux=(21-heure)/2
  	  ksoleil=0.6074*aux+(1-aux)*0.4
  EndIf	
  
     Var b=ksoleil^1.4:b=1.15+(ksoleil-1)*0.6'0.5
     Var c=min(1.0,b)
     Var a=c*c*0.471'0.65'0.14'0.07
     'a=0
     Var sb=b
     sb=0
     tdark=0
     If heure>20+2 Or heure<6 Then
     	  tdark=1
     	  a=0.01:b=0.35:c=b
     EndIf
     Var sb3=0.9
     gllightfv(gl_light0,GL_ambient,glparam4f(a,a,a, 1)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
     'gllightfv(gl_light0,GL_diffuse,glparam4f(1,1,1, 1)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
     gllightfv(gl_light0,GL_diffuse,glparam4f(b,b,c, 1)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
     gllightfv(gl_light0,GL_specular,glparam4f(sb,sb,sb,1)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
     gllightfv(gl_light3,GL_specular,glparam4f(sb3,sb3,sb3,1)) 'GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
 
  Var x=0-800*(12-heure),y=6000,z=490*(12-abs(12-heure))
  aux=0.8*10000*4*(1-Abs(12-heure)/140)/Sqr(x*x+y*y+z*z)
  kxsoleil=aux*x:kysoleil=aux*y:kzsoleil=aux*z
  suno1=diro1(kxsoleil,kysoleil)
  suntan2=Sqr(x*x+y*y)/max(0.001,z)

   gllightfv(gl_light0,GL_position,glparam4f(mx-kxsoleil*9,my-kysoleil*9,mz+kzsoleil*9,0))'w=0,directional
   glLightf(gl_light0, GL_SPOT_EXPONENT, 0.0)'3.0)
   glLightf(gl_light0, GL_CONSTANT_ATTENUATION, 0.0)
   glLightf(gl_light0, GL_LINEAR_ATTENUATION, 0.0)
   glLightf(gl_light0, GL_QUADRATIC_ATTENUATION, 0.0)

End Sub
Dim Shared As uint sunsettext
Sub drawsunset
If sunsettext=0 Then sunsettext=guiloadtexture(ExePath+"/media/sun.jpg")
Dim As Single aux
aux=ksoleil'0.5
'glClearColor 0.5*aux, 0.5*aux, 1.0*aux, 0.0
glEnable GL_BLEND
glBlendFunc GL_SRC_color,GL_ONE'_MINUS_SRC_color
glpushmatrix
'gltranslatef( mx+40000,my,mz-5000)
Var k5=2.5
gltranslatef( mx-kxsoleil*k5,my-kysoleil*k5,mz+kzsoleil*k5)'5
glrotatef(-heure*170/24,0,0,1)
glrotatef(-50*(1-Abs(heure-12)/12),0,1,0)
gldisable GL_LIGHTING
'glenable GL_LIGHTING
gldisable GL_DEPTH_TEST
'glEnable GL_COLOR_LOGIC_OP
glEnable GL_TEXTURE_2D
'glLogicOp GL_OR
glbindtexture(gl_texture_2d,sunsettext)
aux=210/256'184/256'170/256
glcolor3f(aux,aux,aux*0.6)
'glcolor3f( aux,aux*ksoleil,aux*ksoleil)
gltexcarre3(20000*k5,18000*k5)
'glDisable GL_COLOR_LOGIC_OP
glEnable GL_DEPTH_TEST
gldisable GL_BLEND
If tdark=1 Then glenable GL_LIGHTING
glpopmatrix
End Sub
Dim Shared As Single skydomex(40,40),skydomey(40,40),skydomez(40,40)
Dim Shared As Single skydometx(40,40),skydomety(40,40)
Dim Shared As Single skydomer(40,40),skydomeg(40,40),skydomeb(40,40),o1sky
Dim Shared As Integer tinitskydome=0
Dim Shared As uint skydometext
Sub drawskydome(rx As Single,ix0 As Integer,iy0 As Integer)
Dim As Integer i,j,k,l,ix,iy
Dim As Single do1,do2,r,g,b,kdo1,kdo2,r0,g0,b0,rx0
If skydometext=0 Then skydometext=guiloadtexture(ExePath+"/media/skydome1.jpg")
glbindtexture gl_texture_2D,skydometext
ix=max2(4,min2(40,ix0))
iy=max2(4,min2(40,iy0))
rx0=49000
If tinitskydome=0 Then 
 tinitskydome=1
 For i=0 To ix
	For j=0 To iy
		skydometx(i,j)=i/ix
		skydomety(i,j)=j/iy
		skydomer(i,j)=1
		skydomeg(i,j)=j/iy
		skydomeb(i,j)=j/iy
		do1=360*degtorad*i/ix
		do2=(-90+180*j/iy)*degtorad
		skydomex(i,j)=rx0*Cos(do1)*Cos(do2)
		skydomey(i,j)=rx0*Sin(do1)*Cos(do2)
		skydomez(i,j)=rx0*Sin(do2)
	Next
 Next
EndIf  
'glcolor3f Sqr(ksoleil), ksoleil, min(1.0,2.0*ksoleil)
setksoleil()
Var ksoleil1=ksoleil
Var heure00=14.0,heure6=6.0,heure20=20.0
tdark=0
If heure<6 Or heure>20 Then
	tdark=1
EndIf
If heure>heure6 And heure<heure6+1 Then ksoleil1*=0.4+0.6*(heure-heure6)
r0=Sqr(ksoleil1):g0=ksoleil1:b0=min(1.0,2.0*ksoleil1)
o1sky=Sin(Timer*0.004)*50
glrotatef(o1sky,0,0,1)
'glcolor3f(1,1,1)
'glcolor3f ksoleil, ksoleil, min(1.0,2.0*ksoleil)
For i=0 To ix
   For j=0 To iy
   	'o1sky=0
		do1=(360*i/ix)+o1sky+50'-(timedo1)
		do2=(-90+180*j/iy)'-(timedo2)
		If (heure<heure20+0.1 and heure>heure20-2) Then
   		do1+=90	
		EndIf
      While do1<-180:do1+=360:Wend
      While do1>180:do1-=360:wend
      While do2<-180:do2+=360:Wend
      While do2>180:do2-=360:Wend
      Var kkdo2=1.0
      If (heure>heure6 And heure<heure6+2)Or(heure<heure20+0.1 and heure>heure20-2) Then kkdo2=max(0.0,min(1.0,(-80+180*j/iy)/60))
      kdo1=max(0.0,min(1.0,1-Abs(do1/150)))
      kdo2=max(0.0,min(1.0,(1-Abs(do2/70))*kkdo2))
   	r=max(0.0,min(1.0,r0*(0.6+kdo1*kdo2)))
		skydomer(i,j)=max(0.0,min(1.0,0.7+1.2*(r-0.7)))'*max(0.0,min(1.0,Abs(do1)/180))
		skydomeg(i,j)=max(0.0,min(1.0,kkdo2*0.7+1.2*(g0-0.7)))'*max(0.0,min(1.0,Abs(do1)/180))
		skydomeb(i,j)=max(0.0,min(1.0,kkdo2*0.7+1.2*(b0-0.7)))'*max(0.0,min(1.0,Abs(do1)/180))
   Next 		
Next 
glscalef(rx/rx0,rx/rx0,rx/rx0)
glbegin(gl_quads)
For i=0 To ix-1
	For j=0 To iy-1
		k=i:l=j
   	glTexCoord2f(skydometx(k,l),skydomety(k,l))
   	glcolor3f(skydomer(k,l),skydomeg(k,l),skydomeb(k,l))
	   glvertex3f(skydomex(k,l),skydomey(k,l),skydomez(k,l))				
		k=i:l=j+1
   	glTexCoord2f(skydometx(k,l),skydomety(k,l))
   	glcolor3f(skydomer(k,l),skydomeg(k,l),skydomeb(k,l))
	   glvertex3f(skydomex(k,l),skydomey(k,l),skydomez(k,l))				
		k=i+1:l=j+1
   	glTexCoord2f(skydometx(k,l),skydomety(k,l))
   	glcolor3f(skydomer(k,l),skydomeg(k,l),skydomeb(k,l))
	   glvertex3f(skydomex(k,l),skydomey(k,l),skydomez(k,l))				
		k=i+1:l=j
   	glTexCoord2f(skydometx(k,l),skydomety(k,l))
   	glcolor3f(skydomer(k,l),skydomeg(k,l),skydomeb(k,l))
	   glvertex3f(skydomex(k,l),skydomey(k,l),skydomez(k,l))				
	Next
Next
glend() 	
glcolor3f(1,1,1)
End Sub
Const As Integer ntree=180+120
Dim Shared As uint treetext,treetext2,shadowtreetext,shadowtreetext2
Dim Shared As Integer treetype(ntree) 
Dim Shared As Single o1tree,treex(ntree),treey(ntree),treez(ntree)
Dim Shared As Double timetree
Sub drawtrees()	
Dim As Integer i,j
If treetext=0 Then
	treetext=guiloadtexture(ExePath+"/media/palmier.jpg",250)
	treetext2=guiloadtexture(ExePath+"/media/palmier2.jpg",250)
	shadowtreetext=guiloadtexture(ExePath+"/media/palmier.jpg",250,255,4)
	shadowtreetext2=guiloadtexture(ExePath+"/media/palmier2.jpg",250,255,4)
	Randomize(0)
	For i=1 To ntree
		treetype(i)=1-Int(Rnd*1.75)
		treey(i)=my+(Rnd-0.5)*4000
		If i<180 Then
			treex(i)=-20-Rnd*2000
		Else
			treex(i)=-20-1200-Rnd*1000
		EndIf
		treez(i)=0
	Next
	Randomize(Timer)
EndIf
      If Timer>timetree Or Timer<(timetree-99) Then
	     timetree=Timer+Rnd*Rnd 
	     o1tree=Rnd/2.4
      EndIf
      glenable gl_alpha_test
      glAlphaFunc(gl_less,10/254)
      glcolor3f(1,1,1)
 For i= 1 To ntree
 	   glenable gl_alpha_test
      Select Case treetype(i)
      	Case 0:glbindtexture(GL_TEXTURE_2D,treetext)
      	Case 1:glbindtexture(GL_TEXTURE_2D,treetext2)
      End Select
      Var changetree=0,disttree=2500	
      While treex(i)<mx-disttree :treex(i)+=disttree*2:changetree=1:Wend 
      While treex(i)>mx+disttree :treex(i)-=disttree*2:changetree=1:Wend 	
      While treey(i)<my-disttree :treey(i)+=disttree*2:changetree=1:Wend 
      While treey(i)>my+disttree :treey(i)-=disttree*2:changetree=1:Wend
      If treex(i)<0 Then  
    	 glpushmatrix
   	 gltranslatef(treex(i),treey(i),treez(i)-10)
    	 glrotatef(o1tree+(treex(i)+treey(i))/22,0,0,1)
    	 Var scz=1+(int(Abs(treex(i)+treey(i))*3) Mod 50)/380
    	 glscalef(scz,scz,scz)
       Var scale=1.0*min(1.0,0.3-treex(i)/190)
       Var auxy=120*scale,auxz=(120+treetype(i)*20)*scale
       If i>180 Then gltranslatef(0,0,-auxz*0.55)
       gltexcarre2 auxy,auxz
       gltexcarre2rot auxy,auxz,60
       gltexcarre2rot auxy,auxz,120 
       glpopmatrix   
      EndIf
 Next i
 gldisable gl_alpha_test
End Sub 
Sub drawshadowtrees()
Dim As Integer i,j,k 	
 If tdark=1 Then Exit Sub  
 gldisable gl_depth_test
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 For i=1 To ntree
        Select Case treetype(i)
       	 Case 0:glbindtexture(GL_TEXTURE_2D,shadowtreetext)
       	 Case 1:glbindtexture(GL_TEXTURE_2D,shadowtreetext2)
        End Select
        glpushmatrix
   	  gltranslatef(treex(i),treey(i),treez(i)+0.5)
   	  glrotatef(suno1,0,0,1)
   	  glrotatef(90,0,1,0)
        Var scale=1.0*min(1.0,0.3-treex(i)/190)
        Var auxy=120*scale,auxz=(120+treetype(i)*20)*scale
   	  If i>180 Then
   	  	  gltexcarre2(auxy,auxz*suntan2*0.45,1,0.55)        
   	  Else 
   	  	  gltexcarre2(auxy,auxz*suntan2)        
   	  EndIf
        glpopmatrix
 Next i 
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub
Dim Shared As Single x1,y1,z1,x2,y2,z2
Sub rotavion(ByVal x As Single,ByVal y As Single,ByVal z As Single)
 x1=x*cos1+y*sin1
 'y1=0-x*sin1+y*cos1
 'z1=z
 x2=x1*cos2+z*sin2
 'y2=y1
 y2=-x*sin1+y*cos1
 z2=-x1*sin2+z*cos2
End Sub
Function loadlist(fic As String,size As Single=150) As UInteger 
Dim As uInt myobjlist
Dim As ZString*256 objfic
objfic=fic
myobjlist=glgenlists(1)
glnewlist myobjlist,GL_COMPILE 'GL_COMPILE_AND_EXECUTE'4865
If FileExists(objfic) Then
  'load3DS(@objfic,@"",@"")
  load3DSsize(@objfic,@"",@"",size)'150)
EndIf
glendlist	
Return myobjlist
End Function 
Const As Integer nroc=40
Dim Shared As uint distroc=2000,roctext,roclist
Dim Shared As Single rocx(nroc),rocy(nroc),rocz(nroc),roco1(nroc),roco2(nroc),roco3(nroc)
Dim Shared As Single rocscale(nroc),rocdz(nroc)
Sub initroc()
Dim As Integer i
Randomize(0)
For i=1 To nroc
	rocscale(i)=(0.7+Rnd*2)*0.17
	rocdz(i)=(Rnd-0.7)*rocscale(i)*140
	rocx(i)=-50-Rnd*1800
	rocy(i)=(Rnd-0.5)*1900*2
	rocz(i)=rocdz(i)
	roco1(i)=Rnd*360
	roco2(i)=Rnd*360
	roco3(i)=Rnd*360
Next
Randomize(Timer)
End Sub
Sub drawrocs()
Dim As Integer i 
If roctext=0 Then
	roctext=guiloadtexture(ExePath+"/objects/roc.jpg")
   roclist=loadlist(ExePath+"/objects/roc.3ds",150)
	initroc()
EndIf
glcolor3f(1,1,1)
glbindtexture(GL_TEXTURE_2D,roctext)
'gldisable gl_lighting
For i= 1 To nroc
     Var changeroc=0
     While rocx(i)<mx-distroc :rocx(i)+=distroc*2:changeroc=1:Wend 
     While rocx(i)>mx+distroc :rocx(i)-=distroc*2:changeroc=1:Wend 	
     While rocy(i)<my-distroc :rocy(i)+=distroc*2:changeroc=1:Wend 
     While rocy(i)>my+distroc :rocy(i)-=distroc*2:changeroc=1:Wend 
     rotavion(rocx(i)-mx,rocy(i)-my,rocz(i)-mz)
     If x2>(0.9*max(Abs(y2),Abs(z2))-200*rocscale(i)) Then 	
    	glpushmatrix
  		gltranslatef(rocx(i),rocy(i),rocz(i)-3)
    	glrotatef(roco1(i),0,0,1)
    	glrotatef(roco2(i),1,0,0)
    	glrotatef(roco3(i),0,1,0)
    	glscalef(rocscale(i),rocscale(i),rocscale(i))
    	glcalllist roclist
      glpopmatrix
     EndIf  
Next i 
if tdark=1 then glenable gl_lighting
End Sub
Dim Shared As uint shadowroctext
Sub drawshadowrocs()
Dim As Integer i,j,k 	
 If shadowroctext=0 Then shadowroctext=guiloadtexture(ExePath+"/media/shadowroc.bmp") 
 If tdark=1 Then Exit Sub 
 gldisable gl_depth_test
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 glbindtexture(GL_TEXTURE_2D,shadowroctext)
 For i=1 To nroc
        glpushmatrix
   	  gltranslatef(rocx(i),rocy(i),0.5)
   	  glrotatef(suno1,0,0,1)
   	  glrotatef(90,0,1,0)
        Var scale=rocscale(i)
        Var auxy=2*150*scale,auxz=150*scale+rocz(i)
  	  	  gltexcarre2(auxy,auxz*suntan2*(1+1/(1+suntan2)),1,(2*150*scale-auxz)/(2*150*scale))        
        glpopmatrix    
 Next i 
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub













