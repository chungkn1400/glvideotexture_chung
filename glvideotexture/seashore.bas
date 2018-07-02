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
#Inclib "./loadobj_chung"
Declare Function loadobj Cdecl Alias "loadobj" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr) As Integer        
'call with autoresize desired size=integer 
Declare Function loadobjsize Cdecl Alias "loadobjsize" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr, ByVal size As uint) As Integer        
Declare Function loadobjsizesmooth Cdecl Alias "loadobjsizesmooth" (Byval ficload As ZString Ptr,ByVal textlist As ZString Ptr,_ 
         Byval defauttext As ZString Ptr, ByVal size As uint) As Integer        
Type vertex_type
    As single x,y,z
End Type
'polygon (triangle), 3 numbers that aim 3 vertices
Type polygon_type
    As Integer a,b,c,flags
End Type 
'mapcoord type, 2 texture coordinates for each vertex
Type mapcoord_type
    As single u,v
End Type 
'myobject type
#Define max_obj 200
#Define max_vertices 64000
#Define max_polygons 64000
#Define max_textures 64000
Type  myobj_type
    As ZString*200 nameobj(max_obj-1)
    As Integer obj_poly(max_obj-1)

    As Integer vertices_qty
    As Integer polygons_qty
    As Integer textures_qty

    As vertex_type vertex(max_vertices-1)
    As polygon_type polygon(max_polygons-1)
    As mapcoord_type mapcoord(max_textures-1)
End Type
Declare Function load3DSsizeptr Cdecl Alias "load3DSsizeptr" (Byval ficload As ZString Ptr,ByVal myobjptr As myobj_type Ptr,size As Integer) As Integer          
Declare Function draw3DSptr Cdecl Alias "draw3DSptr" (ByVal myobjptr As myobj_type Ptr) As Integer          
Declare Function draw3DSptrsmooth Cdecl Alias "draw3DSptrsmooth" (ByVal myobjptr As myobj_type Ptr) As Integer          
Declare Function load3DSmap Cdecl Alias "load3DSmap" (Byval ficload As ZString Ptr,ByVal map As Integer Ptr) As Integer        


#Inclib "./morphobj3ds_chung"
Declare Function morphobj3DS Cdecl Alias "morphobj3DS" (ByVal myobj As myobj_type Ptr,ByVal myobj1 As myobj_type Ptr,ByVal kx1 As Single, _      
	                                       ByVal myobj2 As myobj_type Ptr,ByVal kx2 As single)As Integer  


'Dim Shared As myobj_type myobj,myobj0,myobj1,myobj2

Dim Shared As Integer i,j,k,l,n,p,xmax,ymax,winx,winy,file,itime,ncloud2
Dim Shared As String resp
Sub mysubquit
	quit=1
End Sub
dim shared as Single sear=1,seag=1,seab=1
Sub subsear
Dim As Integer i  
getcomboindex("win.sear",i)
sear=0.5+(i)*0.01
Sleep 100	
End Sub
Sub subseag
Dim As Integer i  
getcomboindex("win.seag",i)
seag=0.5+(i)*0.01
Sleep 100	
End Sub
Sub subseab
Dim As Integer i  
getcomboindex("win.seab",i)
seab=0.5+(i)*0.01
Sleep 100	
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
Dim Shared As Integer ibikini=1
Sub subbikini
Dim As Integer i  
getcomboindex("win.bikini",i)
ibikini=i
Sleep 200
End Sub
Dim Shared As Double heure=12
Dim Shared As Integer tdark=0
Sub subhour
Dim As Integer i  
getcomboindex("win.hour",i)
i=max2(0,min2(24,(i-1)*2))
heure=i+(heure-2*Int(heure/2))
If heure>=24 Then
	heure-=24
	selectcomboindex("win.hour",1)
EndIf
Sleep 200
End Sub
Dim Shared As Single kwavez=1
Sub subwavez
Dim As Integer i  
getcomboindex("win.wavez",i)
kwavez=max(0.0,min(5.0,(i-1)*0.2))
Sleep 200
End Sub
Sub subdtime()
Dim As Integer i 
guiconfirm("change time of day ?","confirm",resp)
If resp="yes" Then	
	heure=heure+4:If heure>24 Then heure=0
	i=Int(heure/2+0.001)+1
	selectcomboindex("win.hour",i)
	Sleep 500
	subhour()
	Sleep 500
EndIf  	
guisetfocus("win.graph2")
End Sub
Declare Sub setmousemx()
Dim Shared As Double timemouse
Dim Shared As Integer mouseforward,mouseleft,mouseright,mouseback,mousex,mousey,mousedown,mouseup,mouseon
Dim Shared As Single mousemx=999999,mousemy,mousemz,mousez,mouseo1,mouseo2
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
mousemx=99999
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
If Timer<timemouse+0.2 Then
	setmousemx()
EndIf
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
Sub subrightmouse
Sleep 200	
guisetfocus("win.graph2")	
End Sub
Declare Sub resetbikini()
Dim Shared As Single mx,my,mz,yh,dmx,dmy,dmz,heado1
Dim Shared As Single o1,o2,o3,cos1=1,sin1,cos2=1,sin2,cos3=1,sin3,o22,o33,z22,tcos1=1,tsin1=0
Sub subreset
	mx=0:my=0:mz=0:o1=0:o2=0:o3=0
	cos1=1:sin1=0:cos2=1:sin2=0:cos3=1:sin3=0
	resetbikini()
	Sleep 200
	guisetfocus("win.graph2")
End Sub
Dim Shared As Double time0
const As String crlf=Chr(13)+Chr(10) 
Sub subhelp
Var msg="scale => sea scale"
msg+=crlf+"blue...red => bikini color"
msg+=crlf+"wavez => deep ocean wave heights"
msg+=crlf+"arrows => move"
msg+=crlf+"pageup,down => look up/down"
msg+=crlf+"C => canoe,sail on/off"
msg+=crlf+"Z,S => fly up/down"
msg+=crlf+"S  => swim"
msg+=crlf+"A,E,Q,D  =>  turn head (sail mode)"
msg+=crlf+"F1 => help"
msg+=crlf+"F3 => change time"
msg+=crlf+"ctrl+C => number of clouds"
guinotice msg
time0=timer
End Sub
Dim Shared As Integer tcanoe
Dim Shared As Single canoex,canoey,canoez,canoeo1,canoeo2,canoeo3,shipx,shipy,shipz,shipo1,shipo2,shipo3
Sub subcanoe
tcanoe=(tcanoe+1)Mod 3
If tcanoe=2 And mx<100 Then tcanoe=0
shipo1=canoeo1
shipo2=canoeo2
shipo3=canoeo3
shipx=canoex
shipy=canoey
shipz=canoez
Sleep 200
guisetfocus("win.graph2")
End Sub
dim shared as string msg 
Sub subresp
resp=guigettext("win2.resp")
End Sub
Sub prompt(ByRef msg0 As String,ByRef resp0 As String)
Dim As Integer x400 
x400=max2(400,Len(msg0)*10)
statictext("win2.msg","msg",10,10,x400,40)
edittext("win2.resp","",@subresp,10,60,x400,20)
openwindow("win2","freewebcar_chung",50,50,x400+50,180)
printgui("win2.msg",msg0)
printgui("win2.resp",resp0)
guisetfocus("win2.resp")
While guitestkey(vk_escape)=0 And guitestkey(vk_return)=0
	guiscan
	Sleep 100
Wend
Sleep 100
guiscan
resp=guigettext("win2.resp")
guiclosewindow("win2")
Sleep 200
guiscan 
End Sub
Const As Integer ncloud=200
Sub subncloud
Dim As Integer i  	
Dim As Integer x,y
	resp=Str(ncloud2)
	msg="number of clouds : enter a number (10.."+Str(ncloud)+")  last="+Str(ncloud2)
	prompt(msg,resp)
	i=Val(resp)
	i=max2(10,min2(ncloud,i))
	If i<>ncloud2 Then
		ncloud2=i
	EndIf
guisetfocus("win.graph2")
End Sub
Sub initsounds()
	Dim As String soundfic
   soundfic="sounds/hello how are you.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias hello",0,0,0)
   soundfic="sounds/ocean.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias ocean",0,0,0)
   soundfic="sounds/nature.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias nature",0,0,0)
   soundfic="sounds/waterwave.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias waterwave",0,0,0)
   soundfic="sounds/waterwave2.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias waterwave2",0,0,0)
   soundfic="sounds/seagull.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias seagull",0,0,0)
   soundfic="sounds/windlong.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias wind",0,0,0)
   soundfic="sounds/subwater.mp3"
   mcisendstring("open "+chr$(34)+soundfic+chr$(34)+" shareable alias subwater",0,0,0)
   mcisendstring("play hello from 0",0,0,0)
   mcisendstring("play ocean from 0 repeat",0,0,0)
   mcisendstring("play nature from 0 repeat",0,0,0)
   'mcisendstring("play wind from 100 repeat",0,0,0)
	mcisendstring("setaudio nature volume to "+Str(Int(10)),0,0,0)
	mcisendstring("setaudio waterwave volume to "+Str(Int(400)),0,0,0)
	mcisendstring("setaudio waterwave2 volume to "+Str(Int(200)),0,0,0)
	mcisendstring("setaudio seagull volume to "+Str(Int(120)),0,0,0)
	mcisendstring("setaudio wind volume to "+Str(Int(160)),0,0,0)
	mcisendstring("setaudio subwater volume to "+Str(Int(600)),0,0,0)
End Sub
Sub closesounds()
   mcisendstring("close hello",0,0,0)
   mcisendstring("close ocean",0,0,0)
   mcisendstring("close nature",0,0,0)
	mcisendstring("close waterwave",0,0,0)
	mcisendstring("close waterwave2",0,0,0)
	mcisendstring("close seagull",0,0,0)
	mcisendstring("close wind",0,0,0)
	mcisendstring("close subwater",0,0,0)
   mcisendstring("close all",0,0,0)
End Sub
Dim Shared As Double twater,twater2,tsubwater 
Sub soundwaterwave(dt As Double=0.7)
If twater>Timer+999 Then twater=Timer 'if midnight
If Timer>(twater+dt) Then
  	twater=Timer
   mcisendstring("play waterwave from 0",0,0,0)
EndIf 
End Sub
Sub soundwaterwave2
If twater2>Timer+999 Then twater2=Timer 'if midnight
If Timer>(twater2+1.7) Then
  	twater2=Timer
   mcisendstring("play waterwave2 from 0",0,0,0)
EndIf 
End Sub
Sub soundsubwater(dt As Double=0.7)
If tsubwater>Timer+999 Then tsubwater=Timer 'if midnight
If Timer>(tsubwater+dt) Then
  	tsubwater=Timer
   mcisendstring("play subwater from 2500",0,0,0)
EndIf 
End Sub
Sub soundseagull
		mcisendstring("play seagull from 0",0,0,0)
End Sub
Dim Shared As Integer tsoundwind=0
Sub soundwind
	If tsoundwind=0 Then
		tsoundwind=1
		mcisendstring("play wind from 100 repeat",0,0,0)
	EndIf
End Sub
Sub stopsoundwind
	If tsoundwind=1 Then
		tsoundwind=0
		mcisendstring("stop wind",0,0,0)
	EndIf
End Sub
initsounds()
'soundseagull


Dim As String ficin
Dim As String ficini="seashore.ini"
file=FreeFile
Open ficini For Input As #file
mx=0:my=0:mz=0:o1=0:o2=0:o3=0
If Not Eof(file) Then Line Input #file,ficin:mx=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:my=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:mz=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:o1=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:o2=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:o3=Val(ficin)
ibikini=1
If Not Eof(file) Then Line Input #file,ficin:ibikini=Val(ficin)
kwavez=1
If Not Eof(file) Then Line Input #file,ficin:kwavez=Val(ficin)
tcanoe=0
If Not Eof(file) Then Line Input #file,ficin:tcanoe=Val(ficin)
canoex=80:canoey=0:canoez=0:canoeo1=0
If Not Eof(file) Then Line Input #file,ficin:canoex=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:canoey=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:canoez=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:canoeo1=Val(ficin)
sear=1:seag=1:seab=1
If Not Eof(file) Then Line Input #file,ficin:sear=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:seag=Val(ficin)
If Not Eof(file) Then Line Input #file,ficin:seab=Val(ficin)
ncloud2=150
If Not Eof(file) Then Line Input #file,ficin:ncloud2=Val(ficin)
Close #file

shipo1=canoeo1
shipo2=canoeo2
shipo3=canoeo3

Dim Shared As Integer wx,wy,depth
ScreenInfo wx,wy,depth

xmax=wx-20:ymax=wy-30-256+172
guibackgroundcolor(0,0,100)
guistatictextinkcolor(200,200,200)
button("win.quit","quit",@mysubquit,10,10,50,20)
'button("win.pause","pause",@subpause,70,10,50,20)
i=0:If xmax<1184 Then i=1010-220
combobox("win.sear",@subsear,1010-i,10,51,500)
combobox("win.seag",@subseag,1067-i,10,51,500)
combobox("win.seab",@subseab,1124-i,10,51,500)
statictext("win.msg","",72,12,300,20)
combobox("win.scale2",@subscale2,400,10,80,400)
'combobox("win.image",@subimage,495,10,80,400)
combobox("win.hour",@subhour,495,10,60,500)
combobox("win.bikini",@subbikini,565,10,80,500)
combobox("win.wavez",@subwavez,660,10,90,500)
button("win.reset","reset",@subreset,765,10,70,20)
button("win.help","help",@subhelp,845,10,70,20)
button("win.canoe","canoe",@subcanoe,925,10,70,20)
graphicbox("win.graph2",2,35,xmax,ymax,"opengl")
openwindow("win","seashore",4,4,xmax+10,70+ymax)

trapclose("win",@mysubquit)
trapLeftmouse("win.graph2",@subleftmouse)
trapLeftmouseup("win.graph2",@subleftmouseup)
trapmovemouse("win.graph2",@submovemouse)
trapRightmouse("win.graph2",@subrightmouse)

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

addcombo("win.bikini","white")
addcombo("win.bikini","green")
addcombo("win.bikini","blue")
addcombo("win.bikini","red")
addcombo("win.bikini","yellow")
ibikini=max2(1,min2(5,ibikini))
selectcomboindex("win.bikini",ibikini)
subbikini()
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
heure=Val(Left(Time,2))+Val(Mid(Time,4,2))/60+0.001
selectcomboindex("win.hour",Int(heure/2)+1)
subhour()

For i=0 To 40 Step 2
	addcombo("win.wavez","wavez"+Left(Str(i/10)+".0",3))
Next
selectcomboindex("win.wavez",1+Int(kwavez/0.2+0.001))
subwavez()

For i=0 To 49
	addcombo("win.sear","R"+Str(i+50))
Next
selectcomboindex("win.sear",Int(sear*100-50+0.1))

For i=0 To 49
	addcombo("win.seag","G"+Str(i+50))
Next
selectcomboindex("win.seag",Int(seag*100-50+0.1))

For i=0 To 49
	addcombo("win.seab","B"+Str(i+50))
Next
selectcomboindex("win.seab",Int(seab*100-50+0.1))

'setforegroundwindow(getguih("win"))
guisetfocus("win.quit")
Dim Shared As hwnd winh,winmsg
winh=getguih("win")
winmsg=getguih("win.msg")

Declare Sub initgl
Declare Sub gldrawtext0(ByRef text As String,ByVal x As Single,ByVal y As Single,ByVal scale As Single=1.0)
Declare Sub display()
Sleep 500
guiscan
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
Dim Shared As uint mygltext(11),mygltextwave(11),mygltextfire(11),firetext,mygltextshadowfire(11),windtext
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
	mygltext(i)=guiloadtexture(ExePath+"/media/seashore/glvideo"+Str(i)+".jpg",200,230,5)
	'mygltext(i)=guiloadtexture(ExePath+"/media/seashore/glvideo"+Str(i)+".jpg",200,230,7)
   guiscan
   printgui("win.msg","load texture "+Str(i))
   Sleep 100
Next
printgui("win.msg",Space(200))
End Sub
Sub inittextureswave(ii As Integer=1)
Dim As Integer i=0
Var nimage=""
If ii>1 Then nimage=Str(ii)
'Var fic=(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
Var fic=(ExePath+"/media/seawave/glvideo"+Str(i)+".jpg")
If FileExists(fic)=0 Then
	guinotice fic+" not found !"
	iimage=iimage0
	selectcomboindex("win.image",iimage)
   Exit Sub 
EndIf
iimage=ii
iimage0=iimage
For i=0 To 11
	If mygltextwave(i)<>0 Then guideletetexture(mygltextwave(i))
	'mygltextwave(i)=guiloadtexture(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
	mygltextwave(i)=guiloadtexture(ExePath+"/media/seawave/glvideo"+Str(i)+".jpg",254,1,5)
   guiscan
   printgui("win.msg","load texturewave "+Str(i))
   Sleep 100
Next
printgui("win.msg",Space(200))
End Sub
Sub inittexturesfire(ii As Integer=1)
Dim As Integer i=0
Var nimage=""
If ii>1 Then nimage=Str(ii)
'Var fic=(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
Var fic=(ExePath+"/media/fire256/glvideo"+Str(i)+".jpg")
If FileExists(fic)=0 Then
	guinotice fic+" not found !"
	iimage=iimage0
	selectcomboindex("win.image",iimage)
   Exit Sub 
EndIf
iimage=ii
iimage0=iimage
For i=0 To 11
	If mygltextfire(i)<>0 Then guideletetexture(mygltextfire(i))
	'mygltextfire(i)=guiloadtexture(ExePath+"/media/image"+nimage+"/glvideo"+Str(i)+".jpg")
	mygltextfire(i)=guiloadtexture(ExePath+"/media/fire256/glvideo"+Str(i)+".jpg",250)'254,1,5)
	If mygltextshadowfire(i)<>0 Then guideletetexture(mygltextshadowfire(i))
	mygltextshadowfire(i)=guiloadtexture(ExePath+"/media/fire256/glvideo"+Str(i)+".jpg",120,255)
   guiscan
   printgui("win.msg","load texturefire "+Str(i))
   Sleep 100
Next
If firetext<>0 Then guideletetexture(firetext)
firetext=guiloadtexture(ExePath+"/media/fire.jpg",250)
If windtext<>0 Then guideletetexture(windtext)
windtext=guiloadtexture(ExePath+"/media/vent.jpg",250)
printgui("win.msg",Space(200))
End Sub
inittextures(iimage)
inittextureswave(iimage)
inittexturesfire(iimage)

Const As Single degtorad=ASin(1)/90
Const As Single radtodeg=90/ASin(1)
Dim Shared As Integer tactive,trun

Dim Shared As Single collidex,collidey,collidez,collidex2,collidey2,collidez2,tswim
Dim Shared  As Single mx0,my0,mz0,o10,mz1,o20,mzsub,ksail=1,windo1
Dim Shared As double time1,time2,dtime=0,fps=1,timemsg,kfps=1
time0=Timer

Randomize(0)
mz=max(mz,8.0)
windo1=(Rnd-0.5)*360

While quit=0 And guitestkey(vk_escape)=0
	guiscan
   
   If tinitimage=1 Then
   	tinitimage=0
   	inittextures(iimage)
   EndIf
   
   Var maxfps=60.0
	Sleep Int(max(2.0,(time1+1/maxfps-Timer)*1000))
	time2=time1
	time1=Timer
	fps+=(1.0/max(0.001,time1-time2)-fps)*0.2
	kfps=30.0/max(1.0,fps)
	If Timer>timemsg+0.2 Then
		timemsg=Timer 
		printguih(winmsg,"fps "+Str(Int(fps))+"          ")
		tactive=0
		If winh=getactivewindow() Then tactive=1
		Var vol=max(5.0,min(1000.0,1000*200/max(200.0,330-mx)))
		mcisendstring("setaudio ocean volume to "+Str(Int(vol)),0,0,0)
		vol=max(5.0,min(1000.0,230*110/max(100.0,1100+mx)))
		mcisendstring("setaudio nature volume to "+Str(Int(vol)),0,0,0)
	EndIf

   If tactive=1 Then
    If guitestkey(vk_f1) Then subhelp()	
    If guitestkey(vk_f3) Then subdtime()	
    Var vv=1.5
    Var tmm=0:If Timer>timemouse+0.2 Then tmm=-1
    If guitestkey(vk_numpad1) Then o1-=3*kfps
    If guitestkey(vk_numpad3) Then o1+=3*kfps
    If guitestkey(vk_numpad8) Or guitestkey(vk_b)Or guitestkey(vk_prior)Or mouseup And tmm Then o2=min(85.0,o2+3*kfps)
    If guitestkey(vk_numpad2) Or guitestkey(vk_n)Or guitestkey(vk_next)Or mousedown And tmm Then o2=max(-85.0,o2-3*kfps)
    If guitestkey(vk_numpad5) Then o2=0
    If tcanoe<>2 Then ksail=1
    If guitestkey(vk_left) Or guitestkey(vk_numpad1) Or mouseleft And tmm Then o1+=3*kfps*ksail
    If guitestkey(vk_right) Or guitestkey(vk_numpad3) Or mouseright And tmm Then o1-=3*kfps*ksail
    If guitestkey(vk_a) Or guitestkey(vk_q) Then heado1=min(120.0,heado1+kfps*1.4)
    If guitestkey(vk_e) Or guitestkey(vk_d) Then heado1=max(-120.0,heado1-kfps*1.4)
    If guitestkey(vk_up) Or guitestkey(vk_down) Then heado1=0
    If guitestkey(vk_c) And guitestkey(vk_control)=0 Then subcanoe():Sleep 200
    If guitestkey(vk_c) And guitestkey(vk_control) Then subncloud():Sleep 200
    If (guitestkey(vk_up) Or (mouseforward And tmm))And trun=0 Then
    	 Var kkfps=kfps:If mx>100 Then kkfps*=0.3*1.4
    	 If tswim=1 Then kkfps*=0.40+0.3*Cos(time1)*Cos(time1)
    	 mx+=vv*cos1*kkfps:my+=vv*sin1*kkfps
    	 If Abs(o2)<13 Or o2>22 Or o2<-50 Then o2=0
    EndIf
    If trun=1 Then
    	 Var kkfps=kfps:If mx>100 Then kkfps*=0.2*1.4
    	 If tswim=1 Then kkfps*=0.15+0.3*Cos(time1)*Cos(time1)
    	 mx+=vv*cos1*kkfps:my+=vv*sin1*kkfps
    	 If Abs(o2)<13 Or o2>22 Or o2<-50 Then o2=0
    EndIf
    If guitestkey(vk_down) Or mouseback And tmm Then
    	 mx-=0.5*vv*cos1*kfps:my-=0.5*vv*sin1*kfps
    	 If Abs(o2)<13 Or o2>22 Or o2<-50 Then o2=0
    EndIf
    If guitestkey(vk_z) Or guitestkey(vk_w) Then
    	If mz<-0.001 Then Sleep 350 
    	mz=max(0.0,min(150.0,mz+kfps*0.5))
    	mz1=0
    EndIf
    If guitestkey(vk_s) Then
    	 mz1=max(-12.0,mz1-kfps*0.5)    	 
    	 mz=max(-18.0,mz-kfps) 
    	 If tswim=1 Then
    	 	 mzsub=max(-1.0,mzsub-kfps) 
          soundwaterwave()   	 
    	 EndIf	 
    EndIf
    mzsub=min(0.0,mzsub+0.1*kfps)
   If tcanoe=0 Then  
    Var cz=max(-3.0,collidez-12),dz=0.0
    If mx>160 Then dz=Cos(time1*3.1416)*0.35
    Var kcos1=cos1:If mx<250 Then kcos1=1
  	 Var mzz=min(0.0,(max(-12.0,cz+mz1)))*max(0.0,kcos1)+dz
  	 If mx<110 Or sin2<-0.4 Then mzz=0
  	 If mx<110 Then mz1=0
  	 If tcanoe=1 Then mzz=0:mz1=0
  	 Var x330=1200.0'400.0
  	 If mx>x330 Then mzz=0
  	 If guitestkey(vk_down) Then mzz=0':mz1=0
  	 If guitestkey(vk_up) And tswim=1 And dz<0 Then soundsubwater(5)
  	 If mz1<-0.01 And dz<0 And mx<x330 Then soundwaterwave(1.4)
  	 tswim=0:If mz1<-0.01 And mx<x330 Then tswim=1
  	 If mz<10 Then
  	 	If cos1>0 Or mz>0 Then
  	 	   mz=max(mzz,mz-0.2*kfps)
  	 	Else 
  	 	   mz=max(mzz,mz)
  	 	EndIf 	
  	 EndIf
   EndIf
  EndIf  
   If mx>100 Then
   	If Abs(mx-mx0)+Abs(my-my0)+Abs(mz-mz0)+Abs(o10-o1)>1 Then
   		mx0=mx:my0=my:mz0=mz:o10=o1
   		If tcanoe=0 Then
   			soundwaterwave()
   		ElseIf canoeo2>o20+kfps*0.5 Then  
   			soundwaterwave(1.5)
   		EndIf
   		o20=canoeo2
   	EndIf
   EndIf

   display()
	guirefreshopenGL()
 
	'If Abs(auxvar)>0.00001 Then
	'   printguih(winmsg,"aux="+Str(auxvar)+"          ")
	'EndIf
	If guitestkey(vk_escape) Or guitestkey(vk_space) Then
		If winh=getactivewindow() Then quit=1
	EndIf
Wend

quit=1
Sleep 1000
guicloseOpenGL()
closesounds()

guiclosewindow("win")

file=freefile
Open ficini For Output As #file
Print #file,mx
Print #file,my
Print #file,mz
Print #file,o1
Print #file,o2
Print #file,o3
Print #file,ibikini
Print #file,kwavez
Print #file,tcanoe
Print #file,canoex
Print #file,canoey
Print #file,canoez
Print #file,canoeo1
Print #file,sear
Print #file,seag
Print #file,seab
Print #file,ncloud2
Close #file	

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
	gluPerspective   48.0,    xmax/ymax,  2.0, 100000'1700000
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
Dim Shared As Single cursorx,cursory,cursorz,cursorx0,cursory0,cursorz0
Sub glsetcursor0(ByVal x As Single,ByVal y As Single,ByVal z As Single=0)
	cursorx0=x:cursory0=y:cursorz0=z
End Sub
Sub glplacecursor(ByVal x0 As Single,ByVal y0 As Single,ByVal z0 As Single=-40,ByVal scale As Single=1)
   glLoadIdentity()
   Var x=x0+cursorx0,y=y0+cursory0,z=z0+cursorz0
   If Abs(scale-1)>0.001 Then glscalef(scale,scale,scale)
   cursorx=(-0.31+23.7*(-1+2*x/xmax)*(xmax/ymax)*(520/700))
   cursory=-0.25+17.1*(1-2*y/ymax)
   cursorz=z
   glTranslatef (cursorx,cursory,cursorz)
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
Sub setmousemx()
	mousez=(ymax*0.5-mousey)
	mouseo2=o2+(mousez)*48/(ymax*0.5)
	If mouseo2<-o2 Then
		mousez=(mz+yh)*cos2*(2+48/(-mouseo2+0.7-o2))
		mouseo1=o1+(xmax*0.5-mousex)*45/(xmax*0.5)
		mousemx=mx+mousez*Cos(mouseo1*degtorad)
		mousemy=my+mousez*Sin(mouseo1*degtorad)
	EndIf	
End Sub
Sub drawmouse()
	If mousemx>99990 Then Exit Sub
	If max(Abs(mousemx-mx),Abs(mousemy-my))<5 Then
		mousemx=99999:Exit Sub
	EndIf
	Var o10=diro1(mousemx-mx,mousemy-my)
	Var do1=o10-o1
	While do1>180:do1-=360:Wend
	While do1<-180:do1+=360:Wend
	If do1>0.8*kfps Then o1+=0.8*kfps
	If do1<0.8*kfps Then o1-=0.8*kfps
	If do1>28 Then o1+=2*kfps
	If do1<-28 Then o1-=2*kfps
	If Abs(do1)<0.8*kfps Then o1=o10
	cos1=Cos(degtorad*o1)
	sin1=Sin(degtorad*o1)
	Var vv=1.0
	mx+=vv*kfps*cos1:my+=vv*kfps*sin1
	glpushmatrix
	Var dist=(Abs(mousemx-mx)+Abs(mousemy-my))
	gltranslatef(mousemx,mousemy,mousemz+dist/50)
	gldisable gl_texture_2d
	gldisk(10,10+3*(100+dist)/(dist+200),16,16)
	glenable gl_texture_2d
	glpopmatrix
End Sub
Dim Shared As uint sandtext
Sub drawsand
If sandtext=0 Then sandtext=guiloadtexture(ExePath+"/media/sand.jpg")
glbindtexture gl_texture_2D,sandtext
Dim As single dx=-3000,dxx=-2000,dtx=3.5,dty=5,tx=0,ty=0
   Var ddx=2*dx/dtx,ddy=2*0.7*dx/dty
   Var cc=0.65
   glpushmatrix 
   gltranslatef(min(0.0,Int(mx/ddx)*ddx),Int(my/ddy)*ddy,0)
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
Sub setvideotexture(ByVal dtime As Double,ByRef itexture As Integer,ByRef tx As Single,ByRef ty As Single, _  
	                 ByRef dtx As Single,ByRef dty As Single)
	itexture=Int(dtime)Mod 12
	'glbindtexture gl_texture_2D,mygltext(itexture)
	Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	tx=(ix)/2
	ty=(3-iy)/4
	dtx=1/2:dty=0.86/4         
End Sub
Sub drawsea0(ddx As Single=0)
Dim As Integer i,j,k,p
Var dz0=-50.0,dzz0=-50.0,dzz=-50.0,dz=-50.0	
Var dtime=time1-time0
Dim As integer itexture=0
Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
setvideotexture(dtime,itexture,tx,ty,dtx,dty)
glenable gl_texture_2D
glbindtexture gl_texture_2D,mygltext(itexture)
glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_linear)
glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_linear)'NEAREST)'nomipmap
glnormal3f(0,0,1)
var n30=int(30+mz*0.3)
For p=1 To 3+mz/15
glpushmatrix
gltranslatef((p-1)*400,0,0)
For i=-n30 To n30
	glpushmatrix
	Var sc=scale2*1.5
	Var ky=sc*17.8*2*2
	dzz0=dz0
	dz0=-Abs(i*1.9)-90/max(9.0,110-max(0.0,mx)-mz-mz)
	'dz0=-Abs(i*1.9)-90/max(9.0,115-mx)
   glTranslatef (100+ddx,ky*(i/2+Int(my/ky-0.5)),0)
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
	/'Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	Var tx=(ix)/2
	Var ty=(3-iy)/4,dtx=1/2,dty=0.86/4'/
	Var dx=17.9
	Var dxx=0.0
	Var kcos=1.25*(1-cos1*0.25)
	Var kcos2=1.0
	If mx<270 Then kcos2=max(0.0,(5+cos1)/6)
	If mx>330 Then dxx=-dx*kcos*kcos2
	'Var sc=scale2*1.6
	glscalef(sc,sc,1)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz,dx,(dx+dx-Abs(dz*1.6)-7)*kcos2)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz,-dx,(dx+dx-Abs(dzz*1.6)-7)*kcos2)
	glend()
	If mx+ddx>270 Then dxx=-dx*0.5
	If mx>330 Then dxx=-dx*1.1
	Var d7=0.3*dx*Sgn(dzz)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx-d7,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx-d7,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*4,dx-d7,dx*1.4*kcos2)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*4,-dx-d7,dx*1.4*kcos2)
	glend()
	If mx+ddx>270 Then dxx=-dx
	If mx>330 Then dxx=-dx*1.2
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(dzz*2,-dx,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dz*2,dx,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*6,dx,dx*1.4*kcos2)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*6,-dx,dx*1.4*kcos2)
	glend()
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(dzz*6,-dx-d7,-dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dz*6,dx-d7,-dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*10,dx-d7,dx*1.5*kcos2)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dzz*10,-dx-d7,dx*1.5*kcos2)
	glend()
	glpopmatrix
Next i
glpopmatrix
Next p
End Sub
Sub drawsea0_okold(ddx As Single=0)
Dim As Integer i,j,k,p
Var dz0=-50.0,dzz0=-50.0,dzz=-50.0,dz=-50.0	
Var dtime=time1-time0
Dim As integer itexture=0
Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
setvideotexture(dtime,itexture,tx,ty,dtx,dty)
glenable gl_texture_2D
glbindtexture gl_texture_2D,mygltext(itexture)
glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_linear)
glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_linear)'NEAREST)'nomipmap
glnormal3f(0,0,1)
var n30=int(30+mz*0.3)
For p=1 To 3+mz/15
glpushmatrix
gltranslatef((p-1)*400,0,0)
For i=-n30 To n30
	glpushmatrix
	Var sc=scale2*1.5
	Var ky=sc*17.8*2*2
	dzz0=dz0
	dz0=-Abs(i*1.9)-90/max(9.0,115-mx)
   glTranslatef (100+ddx,ky*(i/2+Int(my/ky-0.5)),0)
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
	/'Var dtime1=dtime-Int(dtime)
	Var ix=Int(dtime1*2)
	Var iy=Int((dtime1*2-ix)*4)
	Var tx=(ix)/2
	Var ty=(3-iy)/4,dtx=1/2,dty=0.86/4'/
	Var dx=17.9
	Var dxx=0.0
	Var kcos=1.25*(1-cos1*0.25)
	If mx>330 Then dxx=-dx*kcos
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
	If mx+ddx>270 Then dxx=-dx*0.5
	If mx>330 Then dxx=-dx*1.1
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
	If mx+ddx>270 Then dxx=-dx
	If mx>330 Then dxx=-dx*1.2
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
glpopmatrix
Next p
End Sub
Dim Shared As Single dwave,mxwave,mywave,wavez(50,100),mycos(-10 To 210000)
Dim Shared As Double timewave,timewavez
Dim Shared As Single wavex(50,100,40),wavey(50,100,40),wavedx(50),wavedy(50),wavedz(50)
Sub setwavez()
Dim As Integer i,j,k,n
Dim As Single xx,yy,zz
If mycos(0)<0.01 Then
	For i=-10 To 210000
		mycos(i)=Cos(3.1416*2*i*10/100000)
	Next
	For n=0 To 40
		Var do1=360*(i+(Rnd-0.5)/4)/8
		If n>=8 Then do1+=22.5
		Var scale=1000.0
		wavedx(n)=Cos(do1*degtorad)*scale
		wavedy(n)=Sin(do1*degtorad)*scale
		wavedz(n)=((i Mod 2)*0.5+1)*0.4*((2+Rnd)/2.5)/8
		Var dx=wavedx(n)
		Var dy=wavedy(n)
		Var vx=1+Rnd*0.2
		For i=0 To 50
			For j=0 To 100
				wavex(i,j,n)=(i*dx+j*dy)*vx
			Next
		Next
	Next
EndIf
If Timer<timewavez+0.1 Then Exit Sub
timewavez=max(Timer-0.05,timewavez+0.1)
var n20=20
For i=0 To n20
	For j=0 To 40
		wavez(i,j)=0
	Next
Next
For n=0 To 7' Step 2'2'7
	Var kz=1.0*kwavez
	Var kx=0.9995
	Var dx=wavedx(n)*kfps*kx
	Var dy=wavedy(n)*kfps*kx
	Var dz=wavedz(n)*kz
	For i=0 To n20		
		For j=0 To 40
			xx=wavex(i,j,n)+dx
			while xx>100000:xx-=100000:Wend 
			While xx<0:xx+=100000:Wend 
			wavex(i,j,n)=xx
			wavez(i,j)+=dz*mycos(Int(xx))
		Next
	Next
Next
End Sub
Sub drawseawave(ByVal kblend as single=1)
Dim As Integer i,j,k,ix,iy
If mz>10.1 Then Exit Sub 
setwavez()
If mx>1200 Then
	o22+=(diro1(4,wavez(1,0)-wavez(0,0))-o22)*min(1.0,0.15*kfps) 
	o33+=(diro1(4,wavez(0,0)-wavez(0,1))-o33)*min(1.0,0.15*kfps)
	z22+=(wavez(0,0)-z22)*min(1.0,0.15*kfps)
Else
	o22=0:o33=0:z22=0
EndIf
Var dtime=(time1-time0)
Dim As integer itexture=0
Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
setvideotexture(dtime,itexture,tx,ty,dtx,dty)
glenable gl_texture_2D
glbindtexture gl_texture_2D,mygltextwave(itexture)
glnormal3f(0,0,1)
gldisable gl_depth_test
If kblend<0.999 Then 
	glcolor4f(1,1,1,kblend)
   glEnable GL_BLEND
   glBlendFunc GL_SRC_alpha,GL_ONE_MINUS_SRC_alpha
EndIf 
glpushmatrix
If Timer>timewave+0.05 Then 
 timewave=Timer 
 dwave+=(mx-mxwave)*cos1+(my-mywave)*sin1
 mxwave=mx:mywave=my
EndIf
Var sc=scale2*0.135'0.13
Var d30=sc*250.0
Var d7=7+(dwave-Int(dwave/d30)*d30)
glTranslatef (mx-d7*cos1,my-d7*sin1,0)
glrotatef(Int(o1/30-0.5)*30,0,0,1)
Var n10=10
For i=-n10 To n10
	glpushmatrix
	Var ky=sc*17.8*2*2
   glTranslatef (0,2*ky*(i/2),0)
   If ((i+200) Mod 2)=1 Then
   	glscalef(1,-1,1)
   EndIf
  ' glrotatef(90,0,0,1)
   'glrotatef(90,1,0,0)
   Var txx=tx,dtxx=dtx,tyy=ty,dtyy=dty
   'tx=tyy:dtx=dtyy
   'ty=txx:dty=dtxx
	Var dx=17.9
	Var dxx0=25.0*(0.5+0.5*cos2)+0.07
	Var dz=250.0
	Var dxx=dxx0-0.5,dxxx=dxx0-1
	'Var sc=scale2*1.6
	glscalef(sc,sc*2,1)
	glcolor4f(1,1,1,1)
   Dim As Integer n20=20
   If i>=-1 And i<6 And mx>1200 Then
   	'glcolor4f(1,0,0,1)
   	Dim As Single dtxx,dtyy,txx,tyy,txxx,tyyy
   	dtxx=dtx/n20:dtyy=dty/n20
   	txx=tx:tyy=ty
   	If i<=0 Then dxx-=0.02*kwavez
   	Dim As Single dzz,ddxx,dyy,zzz,zz,xxx,yyy,xx,yy
   	dzz=(dz+5)/n20:dyy=(dxx-dxxx)/n20:ddxx=(dx+dx)/n20
   	zz=-5:xx=-dx:yy=dxxx
      glbegin(gl_quads)
   	For ix=0 To n20-1
   		txxx=txx
   		txx+=dtxx
   		tyy=ty
   		zz=-5
   		xxx=xx
   		xx+=ddxx
   		yy=dxxx
   		For iy=0 To n20-1
   			tyyy=tyy
   			tyy+=dtyy
   			zzz=zz
   			zz+=dzz
   			yyy=yy
   			yy+=dyy
         	'glbegin(gl_quads)
         	glTexCoord2f(txxx,tyyy)
         	'glnormal3f(0,0,1)
         	glvertex3f(zzz,xxx,yyy+wavez(ix,iy))
	         gltexcoord2f(txx,tyyy)
         	'glnormal3f(0,0,1)
	         glvertex3f(zzz,xx,yyy+wavez(ix+1,iy))
	         'glcolor4f(0,1,0,1)
	         glTexCoord2f(txx,tyy)
         	'glnormal3f(0,0,1)
         	glvertex3f(zz,xx,yy+wavez(ix+1,iy+1))
	         gltexcoord2f(txxx,tyy)
         	'glnormal3f(0,0,1)
	         glvertex3f(zz,xxx,yy+wavez(ix,iy+1))
	         'glend()   			
   		Next
   	Next
   	glend()
   	glcolor4f(1,1,1,1)
   Else
    'dxxx-=0.5	
    'glcolor4f(1,0,0,1)
	 glbegin(gl_quads)
	 glTexCoord2f(tx,ty)
	 glvertex3f(-5,-dx,dxxx)
	 gltexcoord2f(tx+dtx,ty)
	 glvertex3f(-5,dx,dxxx)
	 'glcolor4f(0,1,0,1)
	 glTexCoord2f(tx+dtx,ty+dty)
	 glvertex3f(dz,dx,dxx)
	 gltexcoord2f(tx,ty+dty)
	 glvertex3f(dz,-dx,dxx)
	 glend()
	 /'glbegin(gl_quads)
	 glTexCoord2f(tx,ty+dty)
	 glvertex3f(dz,-dx,dxxx)
	 gltexcoord2f(tx+dtx,ty+dty)
	 glvertex3f(dz,dx,dxxx)
	 glTexCoord2f(tx+dtx,ty)
	 glvertex3f(dz+dz,dx,dxx)
	 gltexcoord2f(tx,ty)
	 glvertex3f(dz+dz,-dx,dxx)
	 glend()'/
   EndIf 
   glcolor4f(1,1,1,1)
   If i>=-1 And i<6 And mx>1200 Then
   	'glcolor4f(0,1,0,1)	
	   dxx=dxx0-0.25
	   dxxx=dxx0-0.5
   	If i<=0 Then dxxx-=0.02*kwavez
   	Dim As Single dtxx,dtyy,txx,tyy,txxx,tyyy
   	dtxx=dtx/n20:dtyy=-dty/n20
   	txx=tx:tyy=ty+dty
   	Dim As Single dzz,ddxx,dyy,zzz,zz,xxx,yyy,xx,yy
   	dzz=(dz)/n20:dyy=(dxx-dxxx)/n20:ddxx=(dx+dx)/n20
   	zz=dz:xx=-dx:yy=dxxx
      glbegin(gl_quads)
   	For ix=0 To n20-1
   		txxx=txx
   		txx+=dtxx
   		tyy=ty+dty
   		zz=dz
   		xxx=xx
   		xx+=ddxx
   		yy=dxxx
   		For iy=0 To n20-1
   			tyyy=tyy
   			tyy+=dtyy
   			zzz=zz
   			zz+=dzz
   			yyy=yy
   			yy+=dyy
   			Var iix=ix,iiy=iy+20
         	'glbegin(gl_quads)
         	glTexCoord2f(txxx,tyyy)
         	'glnormal3f(0,0,1)
         	glvertex3f(zzz,xxx,yyy+wavez(iix,iiy))
	         gltexcoord2f(txx,tyyy)
         	'glnormal3f(0,0,1)
	         glvertex3f(zzz,xx,yyy+wavez(iix+1,iiy))
	         'glcolor4f(0,1,0,1)
	         glTexCoord2f(txx,tyy)
         	'glnormal3f(0,0,1)
         	glvertex3f(zz,xx,yy+wavez(iix+1,iiy+1))
	         gltexcoord2f(txxx,tyy)
         	'glnormal3f(0,0,1)
	         glvertex3f(zz,xxx,yy+wavez(iix,iiy+1))
	         'glend()   			
   		Next
   	Next
	   glend()   			
   	glcolor4f(1,1,1,1)
   Else 
	 'glcolor4f(1,0,0,1)
	 'tx=txx:dtx=dtxx
	 'ty=tyy:dty=dtyy
	 dxx=dxx0-0.25
	 dxxx=dxx0-0.5'-0.5
	 glbegin(gl_quads)
	 glTexCoord2f(tx,ty+dty)
	 glvertex3f(dz,-dx,dxxx)
	 gltexcoord2f(tx+dtx,ty+dty)
	 glvertex3f(dz,dx,dxxx)
	 glTexCoord2f(tx+dtx,ty)
	 glvertex3f(dz+dz,dx,dxx)
	 gltexcoord2f(tx,ty)
	 glvertex3f(dz+dz,-dx,dxx)
	 glend()
	EndIf  
	glcolor4f(1,1,1,1)
   'tx=tyy:dtx=dtyy
   'ty=txx:dty=dtxx	
   dxx=dxx0
   dxxx=dxx0-0.25-0.5
   If 0 Then'i>-1 And i<6 And mx>1200 Then
   	'glcolor4f(0,1,0,1)	
   	Dim As Single dtxx,dtyy,txx,tyy,txxx,tyyy
   	dtxx=dtx/n20:dtyy=-dty/n20
   	txx=tx:tyy=ty+dty
   	Dim As Single dzz,ddxx,dyy,zzz,zz,xxx,yyy,xx,yy
   	dzz=(dz*7)/n20:dyy=(dxx-dxxx)/n20:ddxx=(dx+dx)/n20
   	zz=dz+dz:xx=-dx:yy=dxxx
   	For ix=0 To n20-1
   		txxx=txx
   		txx+=dtxx
   		tyy=ty+dty
   		zz=dz
   		xxx=xx
   		xx+=ddxx
   		yy=dxxx
   		For iy=0 To n20-1
   			tyyy=tyy
   			tyy+=dtyy
   			zzz=zz
   			zz+=dzz
   			yyy=yy
   			yy+=dyy
   			Var iix=ix,iiy=iy+40
         	glbegin(gl_quads)
         	glTexCoord2f(txxx,tyyy)
         	glvertex3f(zzz,xxx,yyy+wavez(iix,iiy))
	         gltexcoord2f(txx,tyyy)
	         glvertex3f(zzz,xx,yyy+wavez(iix+1,iiy))
	         'glcolor4f(0,1,0,1)
	         glTexCoord2f(txx,tyy)
         	glvertex3f(zz,xx,yy+wavez(iix+1,iiy+1))
	         gltexcoord2f(txxx,tyy)
	         glvertex3f(zz,xxx,yy+wavez(iix,iiy+1))
	         glend()   			
   		Next
   	Next
   	glcolor4f(1,1,1,1)
   Else	
	 glbegin(gl_quads)
	 glTexCoord2f(tx,ty)
	 glvertex3f(dz+dz,-dx,dxxx)
	 gltexcoord2f(tx+dtx,ty)
	 glvertex3f(dz+dz,dx,dxxx)
	 glTexCoord2f(tx+dtx,ty+dty)
	 glvertex3f(dz*9,dx,dxx)
	 gltexcoord2f(tx,ty+dty)
	 glvertex3f(dz*9,-dx,dxx)
	 glend()
	EndIf  
	glpopmatrix
Next i
glpopmatrix
glcolor4f(1,1,1,1)
gldisable GL_BLEND
glenable gl_depth_test
End Sub
Sub drawseawave_old(ByVal kblend as single=1)
Dim As Integer i,j,k 	
Var dtime=time1-time0
Dim As integer itexture=0
Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
setvideotexture(dtime,itexture,tx,ty,dtx,dty)
glenable gl_texture_2D
glbindtexture gl_texture_2D,mygltextwave(itexture)
glnormal3f(0,0,1)
gldisable gl_depth_test
If kblend<0.999 Then 
	glcolor4f(1,1,1,kblend)
   glEnable GL_BLEND
   glBlendFunc GL_SRC_alpha,GL_ONE_MINUS_SRC_alpha
EndIf 
glpushmatrix
If Timer>timewave+0.05 Then 
 timewave=Timer 
 dwave+=(mx-mxwave)*cos1+(my-mywave)*sin1
 mxwave=mx:mywave=my
EndIf
Var sc=scale2*0.13
Var d30=sc*250.0
Var d7=7+(dwave-Int(dwave/d30)*d30)
glTranslatef (mx-d7*cos1,my-d7*sin1,0)
glrotatef(Int(o1/30)*30,0,0,1)
For i=-10 To 10
	glpushmatrix
	Var ky=sc*17.8*2*2
   glTranslatef (0,2*ky*(i/2),0)
   If ((i+200) Mod 2)=1 Then
   	glscalef(1,-1,1)
   EndIf
  ' glrotatef(90,0,0,1)
   'glrotatef(90,1,0,0)
	Var dx=17.9
	Var dxx=25.0*(0.5+0.5*cos2)
	Var dz=250.0
	'Var sc=scale2*1.6
	glscalef(sc,sc*2,1)
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(0,-dx,dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(0,dx,dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz,dx,dxx)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dz,-dx,dxx)
	glend()
	glbegin(gl_quads)
	glTexCoord2f(tx,ty+dty)
	glvertex3f(dz,-dx,dxx)
	gltexcoord2f(tx+dtx,ty+dty)
	glvertex3f(dz,dx,dxx)
	glTexCoord2f(tx+dtx,ty)
	glvertex3f(dz+dz,dx,dxx)
	gltexcoord2f(tx,ty)
	glvertex3f(dz+dz,-dx,dxx)
	glend()
	glbegin(gl_quads)
	glTexCoord2f(tx,ty)
	glvertex3f(dz+dz,-dx,dxx)
	gltexcoord2f(tx+dtx,ty)
	glvertex3f(dz+dz,dx,dxx)
	glTexCoord2f(tx+dtx,ty+dty)
	glvertex3f(dz*9,dx,dxx)
	gltexcoord2f(tx,ty+dty)
	glvertex3f(dz*9,-dx,dxx)
	glend()
	glpopmatrix
Next i
glpopmatrix
glcolor4f(1,1,1,1)
gldisable GL_BLEND
glenable gl_depth_test
End Sub
/'Sub drawseaband(x0 As Single,x1 As Single,kdy As Single,kty0 As Single,kty1 As Single)
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
End Sub '/
Declare Sub testcollide()
Declare Sub drawtrees()
Declare Sub drawshadowtrees()
Declare Sub drawbushs()
Declare Sub drawshadowbushs()
Declare Sub drawshadowrocs()
Declare Sub drawshadowhelen()
Declare Sub drawshadowkate()
Declare Sub drawseagullshadow()
Declare Sub drawcabaneshadow()
Declare Sub drawcanoeshadow()
Declare Sub drawnshipshadow()
Declare Sub drawrocs()
Declare Sub drawcabane()
Declare Sub drawfire()
Declare Sub drawsmokes()
Declare Sub addsmoke(ByVal mx As Single,ByVal my As Single,ByVal mz As Single,ByVal itype As Integer=0,ByVal vz As Single=0)
Declare Sub drawcanoe()
Declare Sub drawnship()
Declare Sub drawhelene()
Declare Sub drawkate()
Declare Sub drawsunset()
Declare Sub drawsunsetwater()
Declare Sub drawskydome(rx As Single,ix0 As Integer,iy0 As Integer)
Declare Sub drawseagull()
Declare Sub drawclouds() 
Declare Sub drawcloudshadows()
Declare Sub drawgrass() 
Declare Sub drawraindrops()
Declare Sub drawwind()
Dim Shared As Double tdrawraindrops,dtraindrop=2
Sub drawshadows()
gldisable gl_depth_test 
glenable gl_texture_2D
glenable gl_alpha_test
glAlphaFunc(gl_less,100/254)
gldisable(gl_cull_face)
glenable(GL_STENCIL_TEST)
glStencilFunc(GL_always, 1, 255)
glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE)'stencil fail,stencil ok,stencil+depth ok
glColorMask(GL_false, GL_false, GL_false, GL_false)
drawshadowtrees()
drawshadowbushs()
drawshadowrocs()
drawshadowhelen()
drawshadowkate()
drawcabaneshadow()
drawcanoeshadow()
drawnshipshadow()

gldisable gl_texture_2D
gldisable gl_alpha_test
gldisable gl_depth_test
'glbindtexture(gl_texture_2d,whitetext)
'glFrontFace(GL_CCW)
glColorMask(GL_true, GL_true, GL_true, GL_true)
glStencilFunc(GL_notequal, 0, 255)
glStencilOp(GL_KEEP, GL_KEEP, GL_keep)'stencil fail,stencil ok,stencil+depth ok
glEnable GL_BLEND
glBlendFunc GL_zero,GL_src_ALPHA
glpushmatrix
glcolor4f(0.6,0.6,0.6,0.6)
glplacecursor(xmax/2,ymax/2,-40)
glcarre(50)
glpopmatrix
gldisable(GL_STENCIL_TEST)
'glStencilFunc(GL_GEQUAL, 1, &hFF)
'glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE)'stencil fail,stencil ok,stencil+depth ok
'GL_ZERO GL_INCR GL_DECR GL_INVERT
'glStencilMask(&hFF)
'gldisable gl_cull_face
gldisable gl_blend
glenable gl_texture_2d
glenable gl_depth_test
glColorMask(GL_true, GL_true, GL_true, GL_true)
glDepthMask(GL_true)
glcolor4f(1,1,1,1)	
End Sub
Sub drawboussole
Dim As Single rx=12,dx=20,dy=20,scale=0.8,cosx,sinx
glpushmatrix
cosx=cos1:sinx=sin1 
gldrawtext "+", xmax-dx, ymax-dy,scale
gldrawtext "S", xmax-dx+cosx*rx, ymax-dy+sinx*rx,scale
gldrawtext "N", xmax-dx-cosx*rx, ymax-dy-sinx*rx,scale
gldrawtext "W", xmax-dx-sinx*rx, ymax-dy+cosx*rx,scale
gldrawtext "E", xmax-dx+sinx*rx, ymax-dy-cosx*rx,scale
glpopmatrix
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
Dim Shared As Single z23
Sub rotavion2(ByVal x As Single,ByVal y As Single)
 x2=x*cos1+y*sin1
 'y1=0-x*sin1+y*cos1
 'z1=z
 'x2=x1'*cos2+z*sin2
 'y2=y1
 y2=-x*sin1+y*cos1
 'z2=-x1*sin2+z*cos2
End Sub
Dim Shared As Single prop,vprop,nshipx
Sub display()
Dim As Integer i,j,k 

	glClearColor 0.5,0.5,1, 1.0
   glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT  Or GL_STENCIL_BUFFER_BIT)
	
	glloadidentity
		 
   yh=28'50'28
   mx=max(-2400.0-2000,min(2800.0,mx))
   mz=max(-15.0,min(200.0,mz))
   If mz>4 Then
   	o22=0:o33=0:z22=0
   EndIf
   
   cos1=Cos(o1*degtorad):sin1=Sin(o1*degtorad)
   cos2=Cos((o2+o22)*degtorad):sin2=Sin((o2+o22)*degtorad)
   cos3=Cos((o3+o33)*degtorad):sin3=Sin((o3+o33)*degtorad)
   dmx=cos1*cos2:dmy=sin1*cos2:dmz=sin2
   Var do1=o1
   If tcanoe=0 Then heado1=0
   If tcanoe>0 Then do1+=heado1
   tcos1=Cos(do1*degtorad):tsin1=Sin(do1*degtorad)
   Var ddmx=tcos1
   Var ddmy=tsin1
	'glulookat(mx,my,mz+yh, mx+1000*dmx,my+1000*dmy,mz+1000*dmz+yh, 0,0,1)
   glulookat(mx,my,mz+yh+z22+z23+mzsub, mx+1000*ddmx,my+1000*ddmy,mz+1000*dmz+yh+z22+z23+mzsub, -sin3*sin1*0.9985*cos2,sin3*cos1*0.9985*cos2,cos3)

	'dtime=time1-time0
	'itime=Int(dtime)Mod 12
	
	
	If tdark=1 Then glenable gl_lighting

Var cos1save=cos1,sin1save=sin1,o1save=o1
cos1=tcos1:sin1=tsin1:o1=o1+heado1

	glpushmatrix
   gltranslatef(mx,my,0)
   drawskydome(6000,14,14)
   glpopmatrix 	
   drawsunset()
   
   drawclouds()

	drawsand()

glcolor4f(sear,seag,seab,1)	
If mx<1200 Or mz>8 Then 
	Var dmmx=Int(mx/100)*100
	If mx<500 Or mz>8 Then
		dmmx=0
	Else
		dmmx=dmmx-400
	EndIf
	glpushmatrix
	gltranslatef(dmmx,0,0) 
	drawsea0()
	If mx>150 Or mz>15 Then
		drawsea0(70)
	EndIf 	
   If mx>220 Or mz>30 Then
	   drawsea0(140)
   EndIf 	
	glpopmatrix
EndIf
Var tdrawship=0
If mx>nshipx+600 Then tdrawship=1:drawnship()
If mx>590 Then
	Var kblend=max(0.001,(min(mx,1190.0)-590)/600)
   'gldisable gl_lighting
   drawseawave(kblend)
	'If tdark=1 Then glenable gl_lighting
Else
	o22=0:o33=0:z22=0
EndIf
If mx>1200 Then
	soundwind()
Else
	stopsoundwind()
EndIf

   glcolor4f(1,1,1,1)
	drawsunsetwater()
	
	'drawshadows()
	drawshadowrocs()
	drawshadowtrees()
	drawshadowbushs()
	drawshadowhelen()
	drawshadowkate()
	drawseagullshadow()
   drawcabaneshadow()
   drawcanoeshadow()
   drawnshipshadow()
   drawcloudshadows()
	drawtrees()
	drawbushs()
	drawgrass()
   
   'glenable gl_lighting
	drawrocs()
	drawhelene()
	drawkate()
	drawseagull()
	drawcabane()
	If tdrawship=0 Then drawnship()
	drawfire()
	drawsmokes()
   glnormal3f(0,0,1)

   If o2>-45 Then'and mx<100
   	testcollide()
   EndIf
	
	cos1=cos1save:sin1=sin1save:o1=o1save
	drawcanoe()
   cos1=tcos1:sin1=tsin1:o1=o1+heado1
   
	If Timer<tdrawraindrops+dtraindrop Then drawraindrops()
	
   gldisable gl_lighting
   
	    gldisable gl_depth_test
	    
	    drawmouse()
	    
	    drawboussole()
     	 If tcanoe=2 Then drawwind()

	cos1=cos1save:sin1=sin1save:o1=o1save
	    
       If auxtest>0.01 Then  
        If Abs(auxvar)>0.00001 Then gldrawtext("aux= "+Str(auxvar),15,ymax-179,1.2)
        If Abs(auxvar2)>0.00001 Then gldrawtext("aux2= "+Str(auxvar2),15,ymax-199,1.2)
        If Abs(auxvar3)>0.00001 Then gldrawtext("aux3= "+Str(auxvar3),15,ymax-219,1.2)
        If Abs(auxvar4)>0.00001 Then gldrawtext("aux4= "+Str(auxvar4),15,ymax-239,1.2)
        If Abs(auxvar5)>0.00001 Then gldrawtext("aux5= "+Str(auxvar5),15,ymax-259,1.2)
        If Abs(auxvar6)>0.00001 Then gldrawtext("aux6= "+Str(auxvar6),15,ymax-279,1.2)
        If auxtext<>"" Then gldrawtext(auxtext,15,ymax-299,1.2)
       EndIf
       If tcanoe=2 Then
       	gldrawtext("prop= "+Left(Str(prop),4)+"   v="+Left(Str(vprop),4),15,ymax-34,1.2)
       EndIf
       gldrawtext("x= "+Str(Int(mx))+"  y= "+Str(Int(my))+"  z= "+Str(Int(mz+z22)),15,ymax-8,1.2)

	    If time1<time0+60 Then
		    gldrawtext("F3 change time",15,ymax-130,1.2)
		    gldrawtext("B N pageup pagedown",15,ymax-110,1.2)
		    gldrawtext("Z S W",15,ymax-90,1.2)
		    gldrawtext("mouse click",15,ymax-70,1.2)
	    EndIf

	    glenable gl_depth_test
	    
End Sub
Dim Shared As Double tksoleil,heure0
Dim Shared As Single ksoleil,kxsoleil,kysoleil,kzsoleil,suno1,suntan2=1,sunco1=1,sunsi1=0
Dim Shared As Integer iheure,iheure0
Sub setksoleil
  If Timer<tksoleil+10 And Abs(heure-heure0)<1 Then Exit Sub
  Var dtimer=min(12.0,Timer-tksoleil)
  tksoleil=Timer
  heure+=dtimer/3600:If heure>24 Then heure-=24 
  If Abs(heure-heure0)<0.001 Then Exit Sub
  heure0=heure
  iheure=Int(heure/2+0.001)+1
  If iheure<>iheure0 Then
  	  iheure0=iheure
  	  selectcomboindex("win.hour",iheure)
  EndIf
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
     Var mois=Val(Left(Date,2))
     Var dh=1.5/(1+Abs(7-mois)) 
     If heure>20+dh Or heure<5.9-dh Then
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
  sunco1=Cos(suno1*degtorad)
  sunsi1=Sin(suno1*degtorad)

   gllightfv(gl_light0,GL_position,glparam4f(mx-kxsoleil*9,my-kysoleil*9,mz+kzsoleil*9,0))'w=0,directional
   glLightf(gl_light0, GL_SPOT_EXPONENT, 0.0)'3.0)
   glLightf(gl_light0, GL_CONSTANT_ATTENUATION, 0.0)
   glLightf(gl_light0, GL_LINEAR_ATTENUATION, 0.0)
   glLightf(gl_light0, GL_QUADRATIC_ATTENUATION, 0.0)

End Sub
Dim Shared As uint sunsettext,sunbacktext
Sub drawsunset
If sunsettext=0 Then sunsettext=guiloadtexture(ExePath+"/media/sun.jpg")
If sunbacktext=0 Then sunbacktext=guiloadtexture(ExePath+"/media/sunback.jpg",1,255)
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
glpopmatrix
If heure<7.5 Or heure>17.5 Then
   glbindtexture(gl_texture_2d,sunbacktext)
   glBlendFunc GL_SRC_color,GL_ONE_MINUS_SRC_color
   glpushmatrix
   gltranslatef(mx+80000*cos1,my+80000*sin1,mz)
   glrotatef(o1,0,0,1)
   'gltexcarre3(170000,50000,1,1)
   Var xx=100000,yy=15000+kzsoleil*k5*2
	glbegin(gl_quads)
   glcolor4f(1,1,1,1)
	glTexCoord2f(0,0.5)
	glvertex3f(0,-xx,-yy*0.035)
	gltexcoord2f(1,0.5)
	glvertex3f(0,xx,-yy*0.035)
   glcolor4f(0,0,0,0)
	glTexCoord2f(1,1)
	glvertex3f(0,xx,yy)
	gltexcoord2f(0,1)
	glvertex3f(0,-xx,yy)
	glend()
   glcolor4f(1,1,1,1)
   glpopmatrix
EndIf
glEnable GL_DEPTH_TEST
gldisable GL_BLEND
If tdark=1 Then glenable GL_LIGHTING
End Sub
Sub drawsunsetwater
Dim As Single aux
aux=ksoleil'0.5
'glClearColor 0.5*aux, 0.5*aux, 1.0*aux, 0.0
glEnable GL_BLEND
glBlendFunc GL_one_minus_dst_alpha,GL_ONE'_MINUS_SRC_color
glcolormask(1,1,1,0)
glpushmatrix
glscalef(1,1,-1)
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
glpopmatrix
If (heure<7.5 Or heure>17.5)And mx<20 Then
   glbindtexture(gl_texture_2d,sunbacktext)
   glBlendFunc GL_src_color,GL_dst_alpha'SRC_color
   glcolormask(0,0,0,1)
   glpushmatrix
   glscalef(1,1,-1)
   gltranslatef(mx+80000*cos1,my+80000*sin1,mz)
   glrotatef(o1,0,0,1)
   'gltexcarre3(170000,50000,1,1)
   Var xx=100000,yy=15000+kzsoleil*k5*2
	glbegin(gl_quads)
	Var cc=0.93
   glcolor4f(1,1,1,cc)
	glTexCoord2f(0,0.5)
	glvertex3f(0,-xx,-yy*0.01)
	gltexcoord2f(1,0.5)
	glvertex3f(0,xx,-yy*0.01)
   glcolor4f(0,0,0,1)
	glTexCoord2f(1,1)
	glvertex3f(0,xx,yy)
	gltexcoord2f(0,1)
	glvertex3f(0,-xx,yy)
	glend()
   glcolor4f(1,1,1,1)
   glpopmatrix
   glBlendFunc GL_one_minus_dst_alpha,GL_ONE'_MINUS_SRC_color
   glcolormask(1,1,1,0)
   glpushmatrix
   glscalef(1,1,-1)
   gltranslatef(mx+80000*cos1,my+80000*sin1,mz)
   glrotatef(o1,0,0,1)
   'gltexcarre3(170000,50000,1,1)
	glbegin(gl_quads)
   glcolor4f(1,1,1,1)
	glTexCoord2f(0,0.5)
	glvertex3f(0,-xx,-yy*0.01)
	gltexcoord2f(1,0.5)
	glvertex3f(0,xx,-yy*0.01)
   glcolor4f(0,0,0,1)
	glTexCoord2f(1,1)
	glvertex3f(0,xx,yy)
	gltexcoord2f(0,1)
	glvertex3f(0,-xx,yy)
	glend()
   glcolor4f(1,1,1,1)
   glpopmatrix
EndIf
glcolormask(1,1,1,1)
glEnable GL_DEPTH_TEST
gldisable GL_BLEND
If tdark=1 Then glenable GL_LIGHTING
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
 printgui("win.msg","init skydome      ")
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
Var mois=Val(Left(Date,2))
Var dh=1.5/(1+Abs(7-mois)) 
If heure>20+dh Or heure<5.9-dh Then
  tdark=1
EndIf
'If heure>heure6 And heure<heure6+1 Then ksoleil1*=0.4+0.6*(heure-heure6)
If heure>heure6 And heure<heure6+1 Then ksoleil1*=0.65+0.35*(heure-heure6)
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
Const As Integer ntree0=180+120+580
Const As Integer ntree=ntree0+ntree0
Dim Shared As uint treetext,treetext2,shadowtreetext,shadowtreetext2
Dim Shared As Integer treetype(ntree),tshowtree(ntree) 
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
	For i=1 To ntree0
		treetype(i)=1-Int(Rnd*1.75)
		treey(i)=my+(Rnd-0.5)*4000
		If i<180 Then
			treex(i)=-20-Rnd*2000
		ElseIf i<180+120 Then 
			treex(i)=-20-1200-Rnd*1000
		Else
			treex(i)=-20-1900-Rnd*600
		EndIf
		treez(i)=0
	Next
	For j=1 To ntree0
		i=j+ntree0
		treetype(i)=1-Int(Rnd*1.75)
		treey(i)=my+(Rnd-0.5)*4000
		If j<180 Then
			treex(i)=-20-Rnd*2000
		ElseIf j<180+120 Then 
			treex(i)=-20-1200-Rnd*1000
		Else
			treex(i)=-20-1900-Rnd*600
		EndIf
		Var x0=-2500
		treex(i)=x0+(x0-treex(i))
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
 	   glenable gl_alpha_test
 For i= 1 To ntree
      tshowtree(i)=0
 	   If Abs(treex(i)-mx)>2000 And mx<1000 Then Continue For 
 	   If Abs(treex(i)-mx)>3000 And mx>1000 And mx<2000 Then Continue For 
 	   If Abs(treex(i)-mx)>4000 And mx>2000 Then Continue For 
      Var changetree=0,disttree=2500	
      'While treex(i)<mx-disttree :treex(i)+=disttree*2:changetree=1:Wend 
      'While treex(i)>mx+disttree :treex(i)-=disttree*2:changetree=1:Wend 	
      While treey(i)<my-disttree :treey(i)+=disttree*2:changetree=1:Wend 
      While treey(i)>my+disttree :treey(i)-=disttree*2:changetree=1:Wend
      If Abs(treex(i)-mx)+Abs(treey(i)-my)<300 Then tshowtree(i)=1 	
      If treex(i)<0 Then  
       rotavion2(treex(i)-mx,treey(i)-my)
       If x2<0.9*Abs(y2)-160*suntan2 Then Continue For
       tshowtree(i)=1 	
       Select Case treetype(i)
      	Case 0:glbindtexture(GL_TEXTURE_2D,treetext)
      	Case 1:glbindtexture(GL_TEXTURE_2D,treetext2)
       End Select
    	 glpushmatrix
   	 gltranslatef(treex(i),treey(i),treez(i)-10)
    	 glrotatef(o1tree+(treex(i)+treey(i))/22,0,0,1)
    	 Var scz=1+(int(Abs(treex(i)+treey(i))*3) Mod 50)/380
    	 glscalef(scz,scz,scz)
       Var scale=1.0*min(1.0,0.3-treex(i)/190)
       Var auxy=120*scale,auxz=(120+treetype(i)*20)*scale
       If i>180 And i<180+120+220 Then
       	gltranslatef(0,0,-auxz*0.55)
       Else
      	Var ii=ntree0+(ntree0-i)
         If ii>180 And ii<180+120+220 Then gltranslatef(0,0,-auxz*0.55)
       EndIf 
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
 'glenable gl_alpha_test
 'glAlphaFunc(gl_less,100/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 For i=1 To ntree
 	     If tshowtree(i)=0 Then Continue For 
 	     If treex(i)>0 Then Continue For 
        Select Case treetype(i)
        	Case 0:glbindtexture(GL_TEXTURE_2D,shadowtreetext)
        	Case 1:glbindtexture(GL_TEXTURE_2D,shadowtreetext2)
        	 'Case 0:glbindtexture(GL_TEXTURE_2D,treetext)
        	 'Case 1:glbindtexture(GL_TEXTURE_2D,treetext2)
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
Const As Integer nbush0=120+100,nbush=nbush0+nbush0
Dim Shared As uint bushtext,bushtext2,shadowbushtext,shadowbushtext2
Dim Shared As Integer bushtype(nbush),tshowbush(nbush) 
Dim Shared As Single o1bush,bushx(nbush),bushy(nbush),bushz(nbush)
Dim Shared As Double timebush
Sub drawbushs()	
Dim As Integer i,j
If bushtext=0 Then
	bushtext=guiloadtexture(ExePath+"/media/buisson2.jpg",250)
	bushtext2=guiloadtexture(ExePath+"/media/buisson2.jpg",250)
	shadowbushtext=guiloadtexture(ExePath+"/media/buisson2.jpg",250,255,4)
	shadowbushtext2=guiloadtexture(ExePath+"/media/buisson2.jpg",250,255,4)
	Randomize(1)
	For i=1 To nbush0
		bushtype(i)=1
		bushy(i)=my+(Rnd-0.5)*4000
		If i<120 Then
			bushx(i)=-20-Rnd*2000
		Else
			bushx(i)=-20-1700-Rnd*800
		EndIf
		bushz(i)=0
	Next
	For j=1 To nbush0
		i=j+nbush0
		bushtype(i)=1
		bushy(i)=my+(Rnd-0.5)*4000
		If j<120 Then
			bushx(i)=-20-Rnd*2000
		Else
			bushx(i)=-20-1700-Rnd*800
		EndIf
		Var x0=-2500
      bushx(i)=x0+(x0-bushx(i))
		bushz(i)=0
	Next
	Randomize(Timer)
EndIf
      If Timer>timebush Or Timer<(timebush-99) Then
	     timebush=Timer+Rnd*Rnd 
	     o1bush=Rnd/2.4
      EndIf
      glenable gl_alpha_test
      glAlphaFunc(gl_less,10/254)
      glcolor3f(0.6,0.65,0.6)
 	   glenable gl_alpha_test
 For i= 1 To nbush
      tshowbush(i)=0
 	   If Abs(bushx(i)-mx)>2000 Then Continue For 
      Var changebush=0,distbush=2500	
      'While bushx(i)<mx-distbush :bushx(i)+=distbush*2:changebush=1:Wend 
      'While bushx(i)>mx+distbush :bushx(i)-=distbush*2:changebush=1:Wend 	
      While bushy(i)<my-distbush :bushy(i)+=distbush*2:changebush=1:Wend 
      While bushy(i)>my+distbush :bushy(i)-=distbush*2:changebush=1:Wend
      If Abs(bushx(i)-mx)+Abs(bushy(i)-my)<300 Then tshowbush(i)=1 	
      If bushx(i)<0 Then
       rotavion2(bushx(i)-mx,bushy(i)-my)
       If x2<0.9*Abs(y2) Then Continue For
       tshowbush(i)=1 	
       Select Case bushtype(i)
      	Case 0:glbindtexture(GL_TEXTURE_2D,bushtext)
      	Case 1:glbindtexture(GL_TEXTURE_2D,bushtext2)
       End Select
    	 glpushmatrix
   	 gltranslatef(bushx(i),bushy(i),bushz(i)-10)
    	 glrotatef(o1bush+(bushx(i)+bushy(i))/22,0,0,1)
    	 Var scz=1+(int(Abs(bushx(i)+bushy(i))*3) Mod 50)/380
    	 glscalef(scz,scz,scz)
       Var scale=0.5*min(1.0,0.3-bushx(i)/190)
       Var auxy=120*scale,auxz=(120+bushtype(i)*20)*scale*0.6
       gltexcarre2 auxy,auxz
       gltexcarre2rot auxy,auxz,60
       gltexcarre2rot auxy,auxz,120 
       glpopmatrix   
      EndIf
 Next i
 gldisable gl_alpha_test
 glcolor3f(1,1,1)
End Sub 
Sub drawshadowbushs()
Dim As Integer i,j,k 	
 If tdark=1 Then Exit Sub  
 gldisable gl_depth_test
 'glenable gl_alpha_test
 'glAlphaFunc(gl_less,100/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 For i=1 To nbush
 	     If tshowbush(i)=0 Then Continue For 
 	     If bushx(i)>0 Then Continue For 
        Select Case bushtype(i)
        	Case 0:glbindtexture(GL_TEXTURE_2D,shadowbushtext)
        	Case 1:glbindtexture(GL_TEXTURE_2D,shadowbushtext2)
        	 'Case 0:glbindtexture(GL_TEXTURE_2D,bushtext)
       	 'Case 1:glbindtexture(GL_TEXTURE_2D,bushtext2)
        End Select
        glpushmatrix
   	  gltranslatef(bushx(i),bushy(i),bushz(i)+0.5)
   	  glrotatef(suno1,0,0,1)
   	  glrotatef(90,0,1,0)
        Var scale=0.5*min(1.0,0.3-bushx(i)/190)
        Var auxy=120*scale,auxz=(120+bushtype(i)*20)*scale*0.6
  	  	  gltexcarre2(auxy,auxz*suntan2)        
        glpopmatrix
 Next i 
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub
Function loadlist(fic As String,size As Single=150) As UInteger 
Dim As uInt myobjlist
Dim As ZString*256 objfic
objfic=fic
myobjlist=glgenlists(1)
glnewlist myobjlist,GL_COMPILE 'GL_COMPILE_AND_EXECUTE'4865
If FileExists(objfic) Then
  'load3DS(@objfic,@"",@"")
  If Right(objfic,4)=".3ds" Then
  	  load3DSsizesmooth(@objfic,@"",@"",size)'150)
  ElseIf Right(objfic,4)=".obj" Then
  	  loadobjsizesmooth(@objfic,@"",@"",size)'150)
  EndIf
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
	rocx(i)=-50-Rnd*(1800+700)
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
     If rocx(i)>100 Then Continue For 
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
 If shadowroctext=0 Then shadowroctext=guiloadtexture(ExePath+"/media/shadowroc.bmp",250) 
 If tdark=1 Then Exit Sub 
 gldisable gl_depth_test
 'glenable gl_alpha_test
 'glAlphaFunc(gl_gequal,100/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 glbindtexture(GL_TEXTURE_2D,shadowroctext)
 For i=1 To nroc
 	     If rocx(i)>0 Then Continue For 
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
Dim Shared As uint cabanetext,cabanelist,chairtext,chairlist
Dim Shared As Single cabanex,cabaney,cabanez,cabaneo1
Sub drawcabane()
Dim As Integer i 
If cabanetext=0 Then
	cabanetext=guiloadtexture(ExePath+"/objects/cabane.jpg")
   cabanelist=loadlist(ExePath+"/objects/cabane.3ds",118)
	chairtext=guiloadtexture(ExePath+"/objects/chair.jpg")
   chairlist=loadlist(ExePath+"/objects/chair.3ds",26.7)
	cabanex=-400
	cabaney=140
	cabanez=0
	cabaneo1=0
EndIf
z23=0
If Abs(cabanex-mx)<60 Then
   If Abs(cabaney-my)<28 Then
		z23=7
	EndIf
EndIf
Var distcabane=2000
glcolor3f(1,1,1)
glbindtexture(GL_TEXTURE_2D,cabanetext)
'gldisable gl_lighting
     Var changecabane=0
     'While cabanex<mx-distcabane :cabanex+=distcabane*2:changecabane=1:Wend 
     'While cabanex>mx+distcabane :cabanex-=distcabane*2:changecabane=1:Wend 	
     While cabaney<my-distcabane :cabaney+=distcabane*2:changecabane=1:Wend 
     While cabaney>my+distcabane :cabaney-=distcabane*2:changecabane=1:Wend 
     'If cabanex>100 Then Continue For 
     rotavion(cabanex-mx,cabaney-my,cabanez-mz)
     If x2>(0.9*max(Abs(y2),Abs(z2))-200) Then 	
    	glpushmatrix
  		gltranslatef(cabanex,cabaney,cabanez)
    	glrotatef(cabaneo1+90,0,0,1)
    	glcalllist cabanelist
    	glpopmatrix
    	glpushmatrix
  		gltranslatef(cabanex,cabaney,cabanez)
    	gltranslatef(35,20,9)
      glbindtexture(GL_TEXTURE_2D,chairtext)
      glrotatef(-58,0,0,1)
      glcalllist chairlist
      glpopmatrix
     EndIf
If tdark=1 then glenable gl_lighting
End Sub
Dim Shared As Double tfiresmoke
Dim Shared As Single firex,firey,firez
Sub drawfire()
firex=cabanex+198:firey=cabaney+150:firez=-2.5
If time2>tfiresmoke+0.4 Then
	tfiresmoke=time2+(Rnd-0.5)*0.3
	addsmoke(firex+(Rnd-0.5),firey+(Rnd-0.5),firez+20+(Rnd-0.5)*2,0,11+(Rnd-0.5)*2)
EndIf
rotavion(firex-mx,firey-my,firez-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-100 Then
   glenable gl_alpha_test
   glAlphaFunc(gl_less,10/254)
	glbindtexture(gl_texture_2d,firetext)
	glpushmatrix
	gltranslatef(firex,firey,firez)
	Var do1=diro1(firex-mx,firey-my)
	glrotatef(do1,0,0,1)
	gltexcarre2(16,16)
	gldisable gl_alpha_test 

   Var dtime=time1-time0+4
   Dim As integer itexture=0
   Dim As Single tx=0.0,ty=0.0,dtx=1.0,dty=1.0
   setvideotexture(dtime,itexture,tx,ty,dtx,dty)
   glbindtexture gl_texture_2D,mygltextfire(itexture)
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_linear)
   glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_linear)'NEAREST)'nomipmap
	gltranslatef(-1,0,7.7)
   gldisable gl_lighting
   glenable gl_blend
   glblendfunc gl_src_color,gl_one_minus_src_color
   glcolor3f(0.9,0.9,0.5)
   'glscalef(1.3,1.3,1.3)
	gltexcarredtxy(3.8,2,11,tx,ty,dtx,dty)
   If tdark=0 Then
   	glpopmatrix
      'glcolor4f(0.5,0.7,0.8,0.4)
      glcolor4f(1,1,1,0.42)
      glblendfunc gl_zero,gl_one_minus_src_alpha
      glbindtexture gl_texture_2D,mygltextshadowfire(itexture)
   	glpushmatrix 
	   gltranslatef(firex,firey,firez+4.4)
   	glrotatef(suno1,0,0,1)
   	glrotatef(90,0,1,0)
      'glscalef(1.3,1.3,1.3)
   	gltexcarredtxy(3.8,2,11*suntan2,tx,ty,dtx,dty)
   EndIf
   gldisable gl_blend
   glcolor4f(1,1,1,1)
   If tdark=1 Then glenable(gl_lighting)
	glpopmatrix
EndIf
End Sub
Const As Integer nsmoke=50
Dim Shared As uint smoketext 
Dim Shared As Integer ismoke=0,typesmoke(nsmoke)'20
Dim Shared As Double smoketime(nsmoke)
Dim Shared As Single tsmoke=0,smokex(nsmoke),smokey(nsmoke),smokez(nsmoke),smokevz(nsmoke)
Sub addsmoke(ByVal mx As Single,ByVal my As Single,ByVal mz As Single,ByVal itype As Integer=0,ByVal vz As Single=0)
Dim i As Integer
For i=1 To nsmoke
	ismoke+=1
	If ismoke>nsmoke Then ismoke=0
	If typesmoke(ismoke)<2 Then
		Exit For  
	Else 
	   If time2>(smoketime(ismoke)+17) Then Exit For
   EndIf 
Next i
	smokex(ismoke)=mx
	smokey(ismoke)=my
	smokez(ismoke)=mz
   smoketime(ismoke)=time2
   typesmoke(ismoke)=itype
   smokevz(ismoke)=vz	
End Sub
Sub drawsmokes()
Dim As Integer i,j,k
Dim As Double dt,r 
If smoketext=0 Then
	smoketext=guiloadtexture(ExePath+"/media/smoke.bmp")
EndIf
	glenable(gl_texture_2d)
   glEnable GL_BLEND
   glBlendFunc GL_SRC_color,GL_ONE'_MINUS_SRC_ALPHA
	'gldisable GL_DEPTH_TEST
	glDepthMask(GL_false)
	For i=0 To nsmoke
		dt=4.07
		If typesmoke(i)=-1 Then dt=1
		If typesmoke(i)=2 Then dt=17
		If typesmoke(i)=3 Then dt=45'35
		If time2<(smoketime(i)+dt) Then 			
		   If time2<(smoketime(i)-100) Then smoketime(i)=time2+dt 'if midnight
	      glpushmatrix
	      smokez(i)+=smokevz(i)*0.03*kfps
	      gltranslatef(smokex(i),smokey(i),smokez(i))
	      glbindtexture(gl_texture_2d,smoketext)
	      Var cc=0.88*(dt+smoketime(i)-time2+1)/(dt+1)
	      glcolor3f(cc,cc,cc)
	      'If tourelle=0 Then 
	        glrotatef(o1,0,0,1)
	        glrotatef(o2,0,1,0)
	      'Else
	      '  glrotatef(to1+o1,0,0,1)
	      '  glrotatef(to2,0,1,0)
	      'EndIf   	
	      If typesmoke(i)=0 Then 
	      	r=50+30*(time2-smoketime(i))
	      ElseIf typesmoke(i)=-1 Then 
	      	r=40+24*(time2-smoketime(i))
	      Else 
	      	If typesmoke(i)=1 Then 
	      		r=150+90*(time2-smoketime(i))
	      	Else 
	      		r=200+200*(time2-smoketime(i))
	      	EndIf
	      EndIf
	      r*=0.15
	      gltexcarre3(r,r)
	      glpopmatrix
	      If tdark=0 Then
	      	Var ccc=0.4*cc
	      	glcolor3f(ccc,ccc,ccc)
            glBlendFunc GL_zero,GL_ONE_MINUS_SRC_color
	      	glpushmatrix
	      	Var h=smokez(i)*suntan2
	         gltranslatef(smokex(i)+h*sunco1,smokey(i)+h*sunsi1,1)
	         glrotatef(suno1,0,0,1)
	         gltexcarre(r)
	      	glpopmatrix 
	      	glcolor3f(1,1,1)
            glBlendFunc GL_SRC_color,GL_ONE'_MINUS_SRC_ALPHA
	      EndIf
		EndIf
	Next
	glEnable GL_DEPTH_TEST
	glDepthMask(GL_true)
	gldisable gl_blend	
End Sub
Dim Shared As Single windv=1,windo3,windprop,shipv,shipdo1,shipo10,shipoo1,shipdoo1
Dim Shared As uint shiptext,shiplist,shipshadowtext,shipshadowtext2,shipbarretext,shipbarrelist
Dim Shared As uint canoetext,canoelist,canoeshadowtext,canoeshadowtext2
Dim Shared As Integer tup
Dim Shared As Double timeup
Dim Shared As uint winpixZ,winpixr,winpixg,winpixb,winpixa,avgcolor
Dim Shared As uint winpixZ2,winpixr2,winpixg2,winpixb2,winpixa2,avgcolor2
Sub drawcanoe0()
glcolor3f(1,1,1)
glbindtexture(GL_TEXTURE_2D,canoetext)
'gldisable gl_lighting
     canoex=min(85.0-Abs(Cos(canoeo1*degtorad)*40),canoex)
     rotavion(canoex-mx,canoey-my,canoez-mz)
     If x2>(0.9*max(Abs(y2),Abs(z2))-200) Then 	
      'glClear (GL_DEPTH_BUFFER_BIT)
    	glpushmatrix
  		If canoex<100 Then
  			gltranslatef(canoex,canoey,0)
  		Else 	
  			gltranslatef(canoex,canoey,canoez)
  		EndIf
    	glrotatef(canoeo1,0,0,1)
    	glrotatef(canoeo2,0,1,0)
    	glrotatef(canoeo3,1,0,0)
    	glcalllist canoelist
      glpopmatrix
     EndIf  
End Sub
Sub drawcanoe()
Dim As Integer i
If canoetext=0 Then
	canoetext=guiloadtexture(ExePath+"/objects/canoe_low.jpg")
   canoelist=loadlist(ExePath+"/objects/canoe_low.3ds",40)
	shiptext=guiloadtexture(ExePath+"/objects/sailship.jpg",10,255)
   shiplist=loadlist(ExePath+"/objects/sailship.obj",60)
	shipbarretext=guiloadtexture(ExePath+"/objects/sailship0.jpg")
   shipbarrelist=loadlist(ExePath+"/objects/sailship_barre.3ds",3)
EndIf
If tcanoe=0 Then
	drawcanoe0
	If tswim=0 Then trun=0
 Var tt=Timer 	
 If guitestkey(vk_up) Then 
 	If tup=0 Then
 		tup=1:timeup=tt
 	ElseIf tt>timeup+0.3 Then
 		trun=1
 	EndIf
 Else
 	If guitestkey(vk_down) Then trun=0
 	If tup=1 And tt<timeup+1 Then trun=0
 	tup=0
 EndIf
 Var mycolor=(winpixr+winpixg+winpixb)*0.33+10
 'If avgcolor<mycolor Then
 	avgcolor+=(mycolor-avgcolor)*min(0.9,0.05*kfps)
 'Else 
 '	avgcolor+=(mycolor-avgcolor)*min(0.9,0.05*kfps)
 'EndIf
 'avgcolor=max(mycolor*0.8,min(avgcolor,mycolor))
 If mycolor>avgcolor*1.25 Then
    'canoeo2+=(4-canoeo2)*min(0.9,0.1715*kfps)
    soundwaterwave()
    soundwaterwave2()
    If tswim=1 Then
    	soundsubwater(2)
      'Var mycolor2=(winpixr2+winpixg2+winpixb2)*0.33+10
    	if mycolor>avgcolor*1.35 Then'1.4 Then
    		If tdrawraindrops+dtraindrop<Timer Then tdrawraindrops=Timer 
    	EndIf
    EndIf
 EndIf
 Exit Sub  
EndIf
canoex=mx:canoey=my:canoez=max(18.5,collidez+3)
If mx<1200 Then
	canoez+=-7-1-sin2*3
EndIf
If mx<100 Then
	canoez=8
	canoeo1=o1:canoeo2=0:canoeo3=0
	trun=0
Else
 If mz>-0.1 Then canoez=min(canoez,mz+18.5)	
 mz=min(8.0,max(canoez-18.5,mz-0.5*kfps))	
 Var tt=Timer 
 canoeo1=o1+Cos(tt*2)*4
 'canoeo2+=(Cos(-tt*3)*4-canoeo2)*min(0.9,0.3*kfps)
 canoeo2+=(Cos(-tt*3)*2-canoeo2)*min(0.9,0.3*kfps)
 Var mycolor=(winpixr+winpixg+winpixb)*0.33+10
 'If avgcolor<mycolor Then
 	avgcolor+=(mycolor-avgcolor)*min(0.9,0.05*kfps)
 'Else 
 '	avgcolor+=(mycolor-avgcolor)*min(0.9,0.08*kfps)
 'EndIf
 'avgcolor=max(mycolor*0.8,min(avgcolor,mycolor))
 If mycolor>avgcolor*1.19 Then'1.1523 Then
    canoeo2+=(4-canoeo2)*min(0.9,0.1715*kfps)
    soundwaterwave()
 EndIf
 If guitestkey(vk_up) Then 
 	canoeo2+=(Cos(-tt*5.5)*6.5-canoeo2)*min(0.9,0.3*kfps)
 	If tup=0 Then
 		tup=1:timeup=tt
 	ElseIf tt>timeup+0.3 Then
 		trun=1
 	EndIf
 Else
 	If guitestkey(vk_down) Then trun=0
 	If tup=1 And tt<timeup+1 Then trun=0
 	tup=0
 	If trun=1 Then
 	  canoeo2+=(Cos(-tt*5.5)*5.5-canoeo2)*min(0.9,0.3*kfps)	  	
 	EndIf
 EndIf
 If trun=0 Then
 	canoeo3=Sin(tt*2.7)*4
 Else 	
 	canoeo3=Sin(tt*2.8)*5.5
 EndIf
EndIf  
glcolor4f(1,1,1,1)
If tcanoe=1 Then
	glbindtexture(GL_TEXTURE_2D,canoetext)
EndIf
If tcanoe=2 Then
	glenable gl_alpha_test
	glalphafunc(gl_gequal,40/255)
	glbindtexture(GL_TEXTURE_2D,shiptext)
EndIf
     rotavion(canoex-mx,canoey-my,canoez-mz)
     If x2>(0.9*max(Abs(y2),Abs(z2))-200) Then 	
      glenable gl_lighting
      glClear (GL_DEPTH_BUFFER_BIT)
    	glpushmatrix
    	If tcanoe=1 Then 
  		   gltranslatef(canoex,canoey,canoez)
    	   glrotatef(canoeo1,0,0,1)
    	   glrotatef(canoeo2,0,1,0)
    	   glrotatef(canoeo3,1,0,0)
    		glcalllist canoelist
    	EndIf
    	If tcanoe=2 Then
    		mx=max(mx,200.0)
    		While canoeo1>180:canoeo1-=360:shipo1=canoeo1:Wend
    		While canoeo1<-180:canoeo1+=360:shipo1=canoeo1:Wend 
    		shipdo1+=(o1-shipo10-shipdo1*1.01)*kfps*0.1
    		shipo10=o1
    		shipdo1=max(-3.0,min(3.0,shipdo1))
    		o1+=shipdo1*kfps*0.6
    		canoeo2=max(-45.0,min(45.0,canoeo2))
    		canoeo3=max(-45.0,min(45.0,canoeo3))
    		Var k01=0.11
    		shipo1+=(canoeo1-shipo1)*kfps*k01
    		shipo2+=(canoeo2*0.85-shipo2)*kfps*k01
    		shipo3+=(canoeo3*0.85-shipo3)*kfps*k01
    		shipx+=(canoex-shipx)*min(0.9,kfps*0.31)
    		shipy+=(canoey-shipy)*min(0.9,kfps*0.31)
    		shipz+=(canoez-shipz)*min(0.9,kfps*0.31)
  		   Var do1=windo1-shipo1
  		   While do1>180:do1-=360:Wend
  		   While do1<-180:do1+=360:wend
  		   Var windvv=windv*2
  		   If do1>0 Then
  		   	windo3=(90-Abs(do1-90))*windvv*0.06
  		   Else
  		   	windo3=(-90+Abs(do1+90))*windvv*0.06
  		   EndIf
  		   Var co1=Cos(degtorad*(windo1-shipo1))
  		   Var si1=Sin(degtorad*(windo1-shipo1))
  		   Var kvoile=0.8
  		   If co1>0 Then
  		   	windprop=(windvv-shipv*(co1+0.115))*kvoile
  		   ElseIf co1>-0.94 Then 
  		   	windprop=(windvv*(1+1.06*co1)+shipv*(co1-0.115))*kvoile
  		   Else 
  		   	windprop=(windvv*(1+1.19*co1)+shipv*(co1-0.115))*kvoile
  		   EndIf
  		   Var kmass=0.02
  		   shipv+=(windprop-0.1*shipv)*kfps*kmass
  		   shipv=max(-5.0,min(5.0,shipv))
  		   ksail=shipv/5.0
  		   trun=0
  		   prop=windprop:vprop=shipv
  		   Var kkfps=kfps*0.07*2.5
  		   mx+=Cos(degtorad*shipo1)*shipv*kkfps
  		   my+=Sin(degtorad*shipo1)*shipv*kkfps
  		   gltranslatef(shipx,shipy,shipz)
    	   glrotatef(shipo1,0,0,1)
    	   glrotatef(shipo2,0,1,0)
    	   glrotatef(shipo3-windo3,1,0,0)
    		If do1>0 Then
    			gltranslatef(19.25,-0.5,3.5)
    		Else 
    			gltranslatef(19.25,0.5,3.5)
    		EndIf
    		If do1>0 Then glscalef(1,-1,1)
    		glcalllist shiplist
         gldisable gl_alpha_test
	      glbindtexture(GL_TEXTURE_2D,shipbarretext)
    		If do1>0 Then
    			gltranslatef(-14.55,0.24,4.7)
    		Else 
    			gltranslatef(-14.55,0.1,4.7)
    		EndIf
    		If guitestkey(vk_left) Then
    			shipdoo1=min(200.0,shipdoo1+kfps*10)
    		ElseIf guitestkey(vk_right) Then
    			shipdoo1=max(-200.0,shipdoo1-kfps*10)
    		Else
    			shipdoo1+=(0-shipdoo1)*kfps*0.07
    		EndIf
    		If do1>0 Then
    			glrotatef(shipdoo1+shipo3*2.5,1,0,0)
    		Else 
    			glrotatef(-shipdoo1-shipo3*2.5,1,0,0)
    		EndIf
    		glscalef(0.54,0.54,0.54)
    		glcalllist shipbarrelist
    	EndIf
      glpopmatrix
      gldisable gl_alpha_test
      If tdark=0 Then gldisable gl_lighting
     EndIf  
End Sub 
Dim Shared As Single nshipy,nshipz,nshipo1,nshipo2,nshipo3,avgcanoez
Dim Shared As Single nnshipx,nnshipy,nnshipz,nnshipo1,nnshipo2,nnshipo3
Sub drawnship()
Dim As Integer i
avgcanoez+=(canoez-avgcanoez)*kfps*0.003
nshipx=1600:nshipz=canoez-avgcanoez+4.55
If mx<nshipx-400 Then nshipz=canoez
If mx>nshipx+600 Then nshipz=canoez
Var dist=4000
While nshipy>my+dist:nshipy-=dist*1.999:Wend
While nshipy<my-dist:nshipy+=dist*1.999:Wend
nshipo3=Sin(time1*2.7)*4
nshipo2=canoeo2
nshipo1=-90
If Abs(nnshipx-mx)<50 Then
	If Abs(nnshipy-my)<80 Then
		mx-=cos1*5
		my-=sin1*5
		Var dxy=(nnshipx-mx)*sin1-(nnshipy-my)*cos1
		If dxy>0 Then
			o1+=7
		Else
			o1-=7
		EndIf
	EndIf
EndIf
glcolor4f(1,1,1,1)
glbindtexture(GL_TEXTURE_2D,shipbarretext)
     rotavion(nshipx-mx,nshipy-my,nshipz-mz)
     If x2>(0.9*max(Abs(y2),Abs(z2))-300) Then 	
      glenable gl_lighting
    	glpushmatrix
    	If 1 Then'tnship=2 Then
    		nshipo2=max(-45.0,min(45.0,nshipo2))
    		nshipo3=max(-45.0,min(45.0,nshipo3))
    		Var k01=0.11
    		nnshipo1+=(nshipo1-nnshipo1)'*kfps*k01
    		nnshipo2+=(nshipo2*0.85-nnshipo2)*kfps*k01
    		nnshipo3+=(nshipo3*0.85-nnshipo3)*kfps*k01
    		nnshipx+=(nshipx-nnshipx)*min(0.9,kfps*0.31)
    		nnshipy+=(nshipy-nnshipy)*min(0.9,kfps*0.31)
    		nnshipz+=(nshipz-nnshipz)*min(0.9,kfps*0.31)
  		   Var do1=windo1-nnshipo1
  		   While do1>180:do1-=360:Wend
  		   While do1<-180:do1+=360:wend
  		   gltranslatef(nnshipx,nnshipy,nnshipz)
    	   glrotatef(nnshipo1,0,0,1)
    	   glrotatef(nnshipo2,0,1,0)
    	   glrotatef(nnshipo3,1,0,0)
    		Var sc=3.5
    		If do1>0 Then
    			glscalef(sc,-sc,sc)
    		Else
    			glscalef(sc,sc,sc)
    		EndIf
    		glcalllist shiplist
    	EndIf
      glpopmatrix
      gldisable gl_alpha_test
      If tdark=0 Then gldisable gl_lighting
     EndIf  
End Sub 
Dim Shared As uint helentext,shadowhelentext,katetext,shadowkatetext,helenbluetext,helenredtext,helenyellowtext
Dim Shared As uint shadowhelen2text,shadowkate2text
Dim Shared As Integer testhelen,testkate,tshowhelen,tshowkate
Dim Shared As Single helenx,heleny,helenz,heleno1,helenco1,helensi1,helenv=1,disthelen
Dim Shared As myobj_type helenobj,helenobj0,helenobj1,helenobj2 
Dim Shared As Single katex,katey,katez,kateo1,kateco1,katesi1,katev=1,distkate
Sub resetbikini()
	helenx=45
	heleny=my-(100+Rnd*400)*Sgn(Rnd-0.5)
	helenz=0
	heleno1=90*Sgn(my-heleny)
	helenco1=Cos(heleno1*degtorad)
	helensi1=Sin(heleno1*degtorad)	
	katex=34
	katey=heleny-(100+Rnd*400)*Sgn(Rnd-0.5)
	katez=0
	kateo1=90*Sgn(my-katey)
	kateco1=Cos(kateo1*degtorad)
	katesi1=Sin(kateo1*degtorad)
End Sub
Sub drawhelene()
Dim As Integer i,j,k 
If helentext=0 Then
   printgui("win.msg","init helene     ")
	Randomize()
	helentext=guiloadtexture(ExePath+"/objects/helene_bikini.jpg")
	helenbluetext=guiloadtexture(ExePath+"/objects/helene_bikini_blue.jpg")
	helenredtext=guiloadtexture(ExePath+"/objects/helene_bikini_red.jpg")
	helenyellowtext=guiloadtexture(ExePath+"/objects/helene_bikini_yellow.jpg")
	shadowhelentext=guiloadtexture(ExePath+"/objects/heleneshadow.jpg",250,255,4)
	shadowhelen2text=guiloadtexture(ExePath+"/objects/heleneshadow2.jpg",250,255,4)
   load3DSsizeptr(ExePath+"/objects/helene_bikini.3ds",@helenobj0,150)
   load3DSsizeptr(ExePath+"/objects/helene_bikini_walk1.3ds",@helenobj1,150)
   load3DSsizeptr(ExePath+"/objects/helene_bikini_walk2.3ds",@helenobj2,150)
	helenobj=helenobj0
	helenx=45
	heleny=my-(100+Rnd*400)*Sgn(Rnd-0.5)
	helenz=0
	heleno1=90*Sgn(my-heleny)
	helenco1=Cos(heleno1*degtorad)
	helensi1=Sin(heleno1*degtorad)
EndIf
glcolor3f(1,1,1)
If ibikini=1 Then
	glbindtexture(GL_TEXTURE_2D,helentext)
ElseIf ibikini=2 Then
	glbindtexture(GL_TEXTURE_2D,katetext)
ElseIf ibikini=3 Then
	glbindtexture(GL_TEXTURE_2D,helenbluetext)
ElseIf ibikini=4 Then
	glbindtexture(GL_TEXTURE_2D,helenredtext)
Else'If ibikini=5 Then
	glbindtexture(GL_TEXTURE_2D,helenyellowtext)	
EndIf
If heleny>my+1000 Then
	heleny=my+999*Sgn(sin1):heleno1=-90*Sgn(sin1)
	helenco1=Cos(heleno1*degtorad)
	helensi1=Sin(heleno1*degtorad)
EndIf
If heleny<my-1000 Then
	heleny=my+999*Sgn(sin1):heleno1=-90*Sgn(sin1)
	helenco1=Cos(heleno1*degtorad)
	helensi1=Sin(heleno1*degtorad)
EndIf
Var d7=9.0
Var dist=max(Abs(mx-helenx-d7*helenco1),Abs(my-heleny-d7*helensi1))
Var disthelen=max(Abs(mx-helenx),Abs(my-heleny))
testhelen=0
If disthelen<7 Then
	mx-=cos1
	my-=sin1
EndIf
If dist<5 Then
	If disthelen>8.5 Then testhelen=1
Else 	
 Var vv=0.22
 helenx+=helenco1*vv*kfps*helenv
 heleny+=helensi1*vv*kfps*helenv
EndIf 
helenv=1
rotavion(helenx-mx,heleny-my,helenz-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-80 And x2<1000 Then
	glenable gl_lighting
	glpushmatrix
	gltranslatef(helenx,heleny,helenz)
	glrotatef(heleno1+92,0,0,1)
	glrotatef(-2.5,0,1,0)
	Var sc=0.214:glscalef(sc,sc,sc)
	Var kt=1.9*Timer
	Var kx=(1+Cos(kt))*0.8
	morphobj3ds(@helenobj,@helenobj0,kx,@helenobj1,(1-kx))
	Var kx2=(1+Sin(kt))*0.65
	morphobj3ds(@helenobj,@helenobj,kx2,@helenobj2,(1-kx2))
	helenv=kx*0.5+kx2*0.5+0.5
	draw3DSptrsmooth(@helenobj)
   'draw3DSptr(@myobj)
	glpopmatrix	
	If tdark=0 Then gldisable gl_lighting
EndIf
End Sub 
Sub drawshadowhelen()
Dim As Integer i,j,k
 If tdark=1 Then Exit Sub  
 gldisable gl_depth_test
 'glenable gl_alpha_test
 'glAlphaFunc(gl_gequal,100/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
	Var kt=1.9*Timer+1
	Var kx=Cos(kt)'(1+Cos(kt))*0.8
	'morphobj3ds(@helenobj,@helenobj0,kx,@helenobj1,(1-kx))
	Var kx2=Sin(kt)'(1+Sin(kt))*0.65
	'morphobj3ds(@helenobj,@helenobj,kx2,@helenobj2,(1-kx2))
	Var ky=1.0
	If heleno1<0 then
	 If (kx)<-0.7 Then
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowhelentext)
	 ElseIf (kx2)<-0.7 Then 
		glbindtexture(GL_TEXTURE_2D,shadowhelentext)
	 Else 	
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowhelen2text)
	 EndIf
	Else
	 If (kx2)<-0.7 Then
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowhelentext)
	 ElseIf (kx)<-0.7 Then 
		glbindtexture(GL_TEXTURE_2D,shadowhelentext)
	 Else 	
		ky=1
		glbindtexture(GL_TEXTURE_2D,shadowhelen2text)
	 EndIf
	EndIf  
        glpushmatrix
   	  gltranslatef(helenx,heleny,helenz+0.5)
   	  glrotatef(suno1,0,0,1)
   	  glrotatef(90,0,1,0)
        Var scale=0.214
        Var auxy=120*scale,auxz=150*scale        
  	  	  gltexcarre2(auxy*ky,auxz*suntan2)        
        glpopmatrix
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub
Sub drawkate()
Dim As Integer i,j,k 
If katetext=0 Then
   printgui("win.msg","init kate     ")
	Randomize()
	katetext=guiloadtexture(ExePath+"/objects/helene_bikini_green.jpg")
	shadowkatetext=guiloadtexture(ExePath+"/objects/heleneshadow.jpg",250,255,4)
	shadowkate2text=guiloadtexture(ExePath+"/objects/heleneshadow2.jpg",250,255,4)
	katex=34
	katey=heleny-(100+Rnd*400)*Sgn(Rnd-0.5)
	katez=0
	kateo1=90*Sgn(my-katey)
	kateco1=Cos(kateo1*degtorad)
	katesi1=Sin(kateo1*degtorad)
EndIf
glcolor3f(1,1,1)
glbindtexture(GL_TEXTURE_2D,katetext)
If katey>my+1000 Then
	katey=my+999*Sgn(sin1):kateo1=-90*Sgn(sin1)
	kateco1=Cos(kateo1*degtorad)
	katesi1=Sin(kateo1*degtorad)
EndIf
If katey<my-1000 Then
	katey=my+999*Sgn(sin1):kateo1=-90*Sgn(sin1)
	kateco1=Cos(kateo1*degtorad)
	katesi1=Sin(kateo1*degtorad)
EndIf
Var d7=9.0
Var dist=max(Abs(mx-katex-d7*kateco1),Abs(my-katey-d7*katesi1))
Var distkate=max(Abs(mx-katex),Abs(my-katey))
testkate=0
If distkate<7 Then
	mx-=cos1
	my-=sin1
EndIf
If dist<5 Then
	If distkate>8.5 Then testkate=1
Else 	
 Var vv=0.22
 katex+=kateco1*vv*kfps*katev
 katey+=katesi1*vv*kfps*katev
EndIf 
katev=1
rotavion(katex-mx,katey-my,katez-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-80 Then
	glenable gl_lighting
	glpushmatrix
	gltranslatef(katex,katey,katez)
	glrotatef(kateo1+92,0,0,1)
	glrotatef(2.5,0,1,0)
	Var sc=0.214:glscalef(-sc,sc,sc)
	Var kt=1.9*Timer+1
	Var kx=(1+Cos(kt))*0.8
	morphobj3ds(@helenobj,@helenobj0,kx,@helenobj1,(1-kx))
	Var kx2=(1+Sin(kt))*0.65
	morphobj3ds(@helenobj,@helenobj,kx2,@helenobj2,(1-kx2))
	katev=kx*0.5+kx2*0.5+0.5
	draw3DSptrsmooth(@helenobj)
   'draw3DSptr(@myobj)
	glpopmatrix	
	If tdark=0 Then gldisable gl_lighting
EndIf
End Sub 
Sub drawshadowkate()
Dim As Integer i,j,k
 If tdark=1 Then Exit Sub  
 gldisable gl_depth_test
 'glenable gl_alpha_test
 'glAlphaFunc(gl_gequal,100/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
	Var kt=1.9*Timer+1
	Var kx=Cos(kt)'(1+Cos(kt))*0.8
	'morphobj3ds(@helenobj,@helenobj0,kx,@helenobj1,(1-kx))
	Var kx2=Sin(kt)'(1+Sin(kt))*0.65
	'morphobj3ds(@helenobj,@helenobj,kx2,@helenobj2,(1-kx2))
	Var ky=1.0
	If kateo1<0 then
	 If (kx)>0.7 Then
		glbindtexture(GL_TEXTURE_2D,shadowkatetext)
	 ElseIf (kx2)<-0.7 Then 
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowkatetext)
	 Else 	
		ky=1
		glbindtexture(GL_TEXTURE_2D,shadowkate2text)
	 EndIf
	Else
	 If (kx2)>0.7 Then
		glbindtexture(GL_TEXTURE_2D,shadowkatetext)
	 ElseIf (kx)>0.7 Then 
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowkatetext)
	 Else 	
		ky=-1
		glbindtexture(GL_TEXTURE_2D,shadowkate2text)
	 EndIf
	EndIf  
        glpushmatrix
   	  gltranslatef(katex,katey,katez+0.5)
   	  glrotatef(suno1,0,0,1)
   	  glrotatef(90,0,1,0)
        Var scale=0.214
        Var auxy=120*scale,auxz=150*scale        
  	  	  gltexcarre2(-auxy*ky,auxz*suntan2)        
        glpopmatrix
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub
Dim Shared As uint seagulltext(5),seagullshadowtext(5)
Dim Shared As Single seagullx,seagully,seagullz,seagullo1
Sub initseagull()
seagullo1=Rnd*360-180
Var co1=Cos(degtorad*seagullo1),si1=Sin(degtorad*seagullo1)
cos1=Cos(degtorad*o1):sin1=Sin(degtorad*o1)
Var r=700
seagullx=mx-r*co1+r*(Rnd-0.5)*0.5
seagully=my-r*si1+r*(Rnd-0.5)*0.5
If (cos1*co1+sin1*si1)>-0.25 Then
	seagullx+=r*cos1*0.75
	seagully+=r*sin1*0.75
EndIf
seagullz=((10+5*Rnd)*5+mz)
End Sub
Dim Shared As Double tsoundseagull
Sub drawseagull()
Dim As Integer i	
If seagulltext(0)=0 Then
  printgui("win.msg","init seagull      ")
  For i=0 To 4
	 seagulltext(i)=guiloadtexture(ExePath+"/media/seagull/seagull"+Str(i+1)+".jpg",250,255)
	 seagullshadowtext(i)=guiloadtexture(ExePath+"/media/seagull/seagull"+Str(i+1)+".jpg",250,255,4)
  Next
  initseagull()	
EndIf
 Var co1=Cos(degtorad*seagullo1),si1=Sin(degtorad*seagullo1)
 Var dxy=(seagullx-mx)*si1-(seagully-my)*co1
 Var v=0.9*kfps
 seagullx+=v*co1:seagully+=v*si1
 'seagullx=mx+580:seagully=my:seagullz=mz
 Var dist=max(Abs(seagullx-mx),Abs(seagully-my))
 If dist<180 Then
 	If Timer>tsoundseagull Then
 		tsoundseagull=Timer+Rnd*Rnd*30
 		soundseagull()
 	EndIf
 EndIf
 If dist>800 Then
 	if Rnd<0.005*kfps Then initseagull()
 EndIf
 seagullz=max(mz+8+dist*0.15,min(mz+dist*0.3,seagullz))
 seagullz=max(25.0,min(400,seagullz))
 glbindtexture(gl_texture_2d,seagulltext(Int(Timer*3.6)Mod 5))
 glenable gl_alpha_test
 glAlphaFunc(gl_less,20/254)
 'gldisable gl_depth_test
 glcolor4f(0.6,0.6,0.6,1)
 glpushmatrix
 'seagullx=mx+500*cos1*cos1:seagully=my+500*sin1*cos1':seagullz=mz+100*cos1
 gltranslatef(seagullx,seagully,seagullz)
 If dxy>0 Then
 	glrotatef(o1,0,0,1)
 Else
 	glrotatef(o1+180,0,0,1)
 EndIf
 gltexcarre2(19,19)
 glpopmatrix
 gldisable gl_alpha_test
 glenable gl_depth_test 
End Sub
Dim Shared As uint raindroptext
Sub drawraindrops()
If raindroptext=0 Then
	 raindroptext=guiloadtexture(ExePath+"/media/raindrops.jpg",150)	
EndIf
 glbindtexture(gl_texture_2d,raindroptext)
 glenable gl_alpha_test
 glAlphaFunc(gl_less,20/254)
 gldisable gl_depth_test
 glcolor4f(1,1,1,1)
 glpushmatrix
 glplacecursor(xmax*0.5,ymax*0.95,-39)
 'gltexcarre(40)
 dtraindrop=2
 Var kt=0.85*(tdrawraindrops+dtraindrop-Timer)/dtraindrop
 Var dx=40.0,dy=20.0,tx=1.2,ty=0.5*2
 dy*=kt:ty*=(kt+0.1)/1.1
 kt=1+(1-kt)*0.2
 dx*=kt
 dy*=kt
	glbegin(gl_quads)
	glTexCoord2f(-tx,1-ty)
	glvertex3f(-dx,-dy,0)
	gltexcoord2f(tx,1-ty)
	glvertex3f(dx,-dy,0)
	glTexCoord2f(tx,1)
	glvertex3f(dx,dy,0)
	gltexcoord2f(-tx,1)
	glvertex3f(-dx,dy,0)
	glend()
 glpopmatrix
 gldisable gl_alpha_test
 glenable gl_depth_test    
End Sub
Dim Shared As Single suno2,sunx,suny,sunz,scalesun=4
Sub drawseagullshadow()
 If tdark=1 Then Exit Sub  
 'seagullx=mx+160:seagully=my:seagullz=100+o1*5
 Var dx=seagullx-mx,dy=seagully-my
 'Var dx1=seagullx-sunx,dy1=seagully-suny
 'Var dxx=-dx1*Cos(degtorad*(suno1-o1))-dy1*Sin(degtorad*(suno1-o1)) 
 Var co1=Cos(degtorad*seagullo1),si1=Sin(degtorad*seagullo1)
 Var dxy=(dx)*si1-(dy)*co1
 glbindtexture(gl_texture_2d,seagullshadowtext(Int(Timer*3.6)Mod 5))
 'glenable gl_alpha_test
 'glAlphaFunc(gl_greater,2/254)
 glenable gl_blend
 glblendfunc gl_zero,gl_one_minus_src_color
 glcolor4f(0.6,0.6,0.6,1) 
 gldisable gl_depth_test
 'glcolor4f(0.05,0.05,0.05,0.55)
 glpushmatrix
 gltranslatef(seagullx+sunco1*seagullz*suntan2*0.3,seagully+sunsi1*seagullz*suntan2*0.3,0.5)
 If dxy>0 Then
 	glrotatef(o1,0,0,1)
 Else
 	glrotatef(o1+180,0,0,1)
 EndIf
 glrotatef(90,0,1,0)
 Var scale=0.214
 Var auxy=80*scale,auxz=150*scale        
 gltexcarre2(-auxy,auxz*suntan2)        
 glpopmatrix
 glcolor4f(1,1,1,1)
 gldisable gl_blend
 glenable gl_depth_test
 gldisable gl_alpha_test
End Sub
Dim Shared As uint cabaneshadowtext
Sub drawcabaneshadow()
If cabaneshadowtext=0 Then
	 cabaneshadowtext=guiloadtexture(ExePath+"/objects/cabaneshadow.jpg",200,255)	
EndIf
If tdark=1 Then Exit Sub  
rotavion(cabanex-mx,cabaney-my,cabanez-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-400 Then
	Var x0=cabanex+90
	Var y0=cabaney-90*sunco1
	Var z0=1
	Var h=130.0
	Var x1=x0+h*sunco1*suntan2
	Var y1=y0+h*sunsi1*suntan2
	Var x2=x1-90-90
	Var y2=y1
	Var x3=x0-90-90
	Var y3=cabaney+90*sunco1
   glenable gl_blend
   glblendfunc gl_zero,gl_one_minus_src_alpha
   glcolor4f(0.6,0.6,0.6,0.6) 
   gldisable gl_depth_test
	glbindtexture(gl_texture_2d,cabaneshadowtext)
	glbegin(gl_quads)
	glTexCoord2f(1,0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,0)
	glvertex3f(x3,y3,z0)
	glend()
   glcolor4f(1,1,1,1)
   gldisable gl_blend
   glenable gl_depth_test
EndIf
End Sub
Sub drawcanoeshadow()
If canoeshadowtext=0 Then
	 canoeshadowtext=guiloadtexture(ExePath+"/objects/canoeshadow.jpg",200,255)	
	 canoeshadowtext2=guiloadtexture(ExePath+"/objects/canoeshadow2.jpg",200,255)	
	 shipshadowtext=guiloadtexture(ExePath+"/objects/sailship_shadow.jpg",200,255)	
EndIf
If tdark=1 Then Exit Sub
rotavion(canoex-mx,canoey-my,canoez-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-300 Then
	Var co1=Cos(canoeo1*degtorad)
	Var si1=Sin(canoeo1*degtorad)
	Var x0=canoex+40*co1
	Var y0=canoey+40*si1
	Var z0=1
	If canoex>100 Then z0=canoez
	Var h=35.0
	Var x1=x0+h*sunco1*suntan2
	Var y1=y0+h*sunsi1*suntan2
	Var x2=x1-(80)*co1
	Var y2=y1-(80)*si1
	Var x3=canoex-40*co1
	Var y3=canoey-40*si1
	If tcanoe=2 Then
		h=80
      Var do3=shipo3-windo3
      Var si3=Sin(degtorad*do3)
      Var si2=Sin(degtorad*shipo2)
		x0=canoex+40*co1
		y0=canoey+40*si1
	   x1=x0+h*(sunco1*suntan2+si1*si3+co1*si2)
	   y1=y0+h*(sunsi1*suntan2-co1*si3+si1*si2)
	   x2=x1-(80)*co1
	   y2=y1-(80)*si1
	   x3=canoex-40*co1
	   y3=canoey-40*si1
	EndIf
   glenable gl_blend
   glblendfunc gl_zero,gl_one_minus_src_alpha
   glcolor4f(0.6,0.6,0.6,0.6) 
   gldisable gl_depth_test
   glDepthMask(GL_true)
  Var do1=canoeo1-suno1	
  Var dco1=Cos(do1*degtorad)
  If Abs(dco1)<=0.983 Then  
	If tcanoe<=1 Then glbindtexture(gl_texture_2d,canoeshadowtext)
	If tcanoe=2 Then glbindtexture(gl_texture_2d,shipshadowtext)
	glbegin(gl_quads)
	glTexCoord2f(1,0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf 	
  Var x10=30.0,y10=7,t0=0.3
If dco1>0.983 Then
  	x0+=-co1*x10+si1*y10
  	y0+=-si1*x10-co1*y10
  	x1+=-co1*x10+si1*y10
  	y1+=-si1*x10-co1*y10
  	x2=x1-si1*2*y10
  	y2=y1+co1*2*y10
  	x3=x0-si1*2*y10
  	y3=y0+co1*2*y10
	glbindtexture(gl_texture_2d,canoeshadowtext2)
	glbegin(gl_quads)
	glTexCoord2f(1,t0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,t0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf
  If dco1<-0.983 Then
  	x0+=-co1*(80-x10)+si1*y10
  	y0+=-si1*(80-x10)-co1*y10
  	x1+=-co1*(80-x10)+si1*y10
  	y1+=-si1*(80-x10)-co1*y10
  	x2=x1-si1*2*y10
  	y2=y1+co1*2*y10
  	x3=x0-si1*2*y10
  	y3=y0+co1*2*y10
	glbindtexture(gl_texture_2d,canoeshadowtext2)
	glbegin(gl_quads)
	glTexCoord2f(1,t0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,t0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf
   glcolor4f(1,1,1,1)
   gldisable gl_blend
   glenable gl_depth_test
EndIf
End Sub
Sub drawnshipshadow()
If tdark=1 Then Exit Sub
rotavion(nshipx-mx,nshipy-my,nshipz-mz)
If x2>0.9*max(Abs(y2),Abs(z2))-500 Then
	Var co1=Cos(nshipo1*degtorad)
	Var si1=Sin(nshipo1*degtorad)
	Var x0=nshipx+40*co1
	Var y0=nshipy+40*si1
	Var z0=1
	If nshipx>100 Then z0=nshipz
	Var h=35.0
	Var x1=x0+h*sunco1*suntan2
	Var y1=y0+h*sunsi1*suntan2
	Var x2=x1-(80)*co1
	Var y2=y1-(80)*si1
	Var x3=nshipx-40*co1
	Var y3=nshipy-40*si1
	If 1 Then
		h=80
      Var do3=shipo3-windo3
      Var si3=Sin(degtorad*do3)
      Var si2=Sin(degtorad*shipo2)
		x0=nshipx+52*co1
		y0=nshipy+52*si1
	   x1=x0+h*(sunco1*suntan2+si1*si3+co1*si2)
	   y1=y0+h*(sunsi1*suntan2-co1*si3+si1*si2)
	   x2=x1-(104)*co1
	   y2=y1-(104)*si1
	   x3=nshipx-52*co1
	   y3=nshipy-52*si1
	EndIf
   glenable gl_blend
   glblendfunc gl_zero,gl_one_minus_src_alpha
   glcolor4f(0.6,0.6,0.6,0.6) 
   gldisable gl_depth_test
   glDepthMask(GL_true)
  Var do1=nshipo1-suno1	
  Var dco1=Cos(do1*degtorad)
  If Abs(dco1)<=0.983 Then  
	glbindtexture(gl_texture_2d,shipshadowtext)
	glbegin(gl_quads)
	glTexCoord2f(1,0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf 	
  Var x10=30.0,y10=7,t0=0.3
  If dco1>0.983 Then
  	x0+=-co1*x10+si1*y10
  	y0+=-si1*x10-co1*y10
  	x1+=-co1*x10+si1*y10
  	y1+=-si1*x10-co1*y10
  	x2=x1-si1*2*y10
  	y2=y1+co1*2*y10
  	x3=x0-si1*2*y10
  	y3=y0+co1*2*y10
	glbindtexture(gl_texture_2d,canoeshadowtext2)
	glbegin(gl_quads)
	glTexCoord2f(1,t0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,t0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf
  If dco1<-0.983 Then
  	x0+=-co1*(80-x10)+si1*y10
  	y0+=-si1*(80-x10)-co1*y10
  	x1+=-co1*(80-x10)+si1*y10
  	y1+=-si1*(80-x10)-co1*y10
  	x2=x1-si1*2*y10
  	y2=y1+co1*2*y10
  	x3=x0-si1*2*y10
  	y3=y0+co1*2*y10
	glbindtexture(gl_texture_2d,canoeshadowtext2)
	glbegin(gl_quads)
	glTexCoord2f(1,t0)
	glvertex3f(x0,y0,z0)
	gltexcoord2f(1,1)
	glvertex3f(x1,y1,z0)
	glTexCoord2f(0,1)
	glvertex3f(x2,y2,z0)
	gltexcoord2f(0,t0)
	glvertex3f(x3,y3,z0)
	glend()
  EndIf
   glcolor4f(1,1,1,1)
   gldisable gl_blend
   glenable gl_depth_test
EndIf
End Sub
Dim Shared As Double timecollide
Sub testcollide()
If time1<timecollide+0.1 Or testhelen=1 Or testkate=1 Then Exit Sub
timecollide=time1
Dim As Integer i,j,k
Dim As integer viewport(4)
Dim As GLdouble modelview(16) 
Dim As GLdouble projection(16)
Dim As glfloat winX, winY, winZ
Dim as GLdouble posX, posY, posZ
glGetDoublev( GL_MODELVIEW_MATRIX, @modelview(0) )
glGetDoublev( GL_PROJECTION_MATRIX, @projection(0) )
glGetIntegerv( GL_VIEWPORT, @viewport(0) )
winx = xmax/2
winy = ymax/3.5
If mx>100 Then winy=ymax/20
If mx>100 Then
	glReadPixels( winx,Int(ymax/10), 1, 1, GL_RGBA, GL_UNSIGNED_byte, @winpixZ )
	winpixa=(winpixz Shr 24)And 255
	winpixb=(winpixz Shr 16)And 255
	winpixg=(winpixz Shr 8)And 255
	winpixr=(winpixz)And 255
	/'glReadPixels( winx,Int(ymax/9), 1, 1, GL_RGBA, GL_UNSIGNED_byte, @winpixZ )
	winpixa2=(winpixz Shr 24)And 255
	winpixb2=(winpixz Shr 16)And 255
	winpixg2=(winpixz Shr 8)And 255
	winpixr2=(winpixz)And 255 '/
	'Exit Sub 
EndIf
glReadPixels( winx,winy, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @winZ )
gluUnProject(winX,winY,winz,@modelview(0),@projection(0),@viewport(0),@posX,@posY,@posZ)   
'glpushmatrix
'gltranslatef(posx,posy,posz)
'gltexsphere 20
'glpopmatrix
Var dist=((posx-mx)*cos1*cos2+(posy-my)*sin1*cos2+(posz-mz)*sin2)
'Var dist=sqr((posx-mx)*(posx-mx)+(posy-my)*(posy-my)+(posz-mz)*(posz-mz))
If dist<12 And dist>0 And mx<100 Then
	mx-=5*cos1
	my-=5*sin1
EndIf
Var collidez0=collidez
collidex=posx
collidey=posy
If max(Abs(collidex-mx),Abs(collidey-my))<30.0 Then
	collidez=posz
EndIf
If collidez-collidez0<-1 And mx>100 Then
	mx+=7*cos1
	my+=7*sin1
EndIf
/'winy = 2
glReadPixels( winx,winy, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @winZ )
gluUnProject(winX,winY,winz,@modelview(0),@projection(0),@viewport(0),@posX,@posY,@posZ)   
collidex2=posx
collidey2=posy
collidez2=posz
'collidez=min(collidez,collidez2)
'/
End Sub
Sub drawcabaneshadowtext()	
  glbindtexture(gl_texture_2d,cabaneshadowtext)
  'gltexparameteri(gl_texture_2d,GL_GENERATE_MIPMAP,gl_true)
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_linear)
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_linear)'NEAREST)'nomipmap
  'glcopyTexImage2D(GL_TEXTURE_2D, 0, gl_rgba,0,0,bmpx,bmpy, 0)   
  glcopyTexSubImage2D(GL_TEXTURE_2D, 0, 0,0, 0,0,512,512)   
End Sub
Dim Shared As uint cloudtext  
Dim Shared As Single cloudx(ncloud),cloudy(ncloud),cloudz(ncloud),cloudr(ncloud)
Dim Shared As Single distcloud=50000/1.3'1.5
Sub initcloud
Dim As Integer i
ncloud2=min2(ncloud,ncloud2)
Randomize(Int(Timer/1000))
For i=1 To ncloud
	cloudx(i)=(Rnd-0.5)*2*distcloud+mx
	cloudy(i)=(Rnd-0.5)*2*distcloud+my
	cloudz(i)=(3000+1200*Rnd)
	cloudr(i)=(1500+Rnd*1000)*2.6
Next
End Sub
Dim Shared As Single rain,krain=1
Dim Shared As Integer ncloud22
Sub drawcloud(ByVal i As Integer)
	      glpushmatrix
	      gltranslatef(cloudx(i),cloudy(i),cloudz(i))	      
	      'glbindtexture(gl_texture_2d,cloudtext)
	      Var cc=1.0,k9000=9000.0
	      If mz>100 Then
	      	cc=min(1.0,max(0.45,0.45*k9000/(4500+Abs(cloudx(i)-mx)+Abs(cloudy(i)-my)+Abs(cloudz(i)-mz))))
	      Else 
	      	cc=min(1.0,max(0.45,0.45*k9000/(4500+Abs(cloudx(i)-mx)+Abs(cloudy(i)-my))))
	      EndIf
	      glcolor3f(cc,cc,cc)
	      'If cc>0.6 And i<=ncloud22 Then
	      	'If Rnd<0.003*0.03*whumidity*kfps Then
	      	'	If (rain<90 Or Rnd<0.01*kfps) Then krain=15:rain=0
	      	'EndIf
	      'EndIf
         glrotatef(o1,0,0,1)
	      glrotatef(-o2,0,1,0)
	      gltexcarre3(cloudr(i)*1.5,cloudr(i))
	      'gltexsphere(800)
	      glpopmatrix
End Sub 
Declare Sub drawrain()
Dim Shared As Single krain0=1
Sub drawclouds()
Dim As Integer i 	
Dim As Integer test
If cloudtext=0 Then
	cloudtext=guiloadtexture(ExePath+"/media/cloud.jpg")
	initcloud()	
EndIf
'krain=0.1
ncloud2=min2(ncloud,ncloud2)
ncloud22=ncloud2'min2(ncloud2,Int(ncloud2*krain0))
If ncloud2>0 Then
If tdark=1 Then
   glenable gl_lighting
Else    
	gldisable gl_lighting
EndIf
'glenable gl_alpha_test
'glAlphaFunc(gl_greater,10/254)
glEnable GL_BLEND
glBlendFunc GL_SRC_color,GL_ONE'_MINUS_SRC_ALPHA
gldisable GL_DEPTH_TEST
'glDepthMask(GL_false)
gldisable gl_alpha_test
glenable(gl_texture_2d)
glbindtexture(gl_texture_2d,cloudtext)
glcolor4f(1,1,1,1)
For i=1 To ncloud2
	test=0
	'cloudx(i)=mx+1000*i:cloudy(i)=my
	cloudx(i)+=kfps*3
	If cloudx(i)<mx-distcloud Then cloudx(i)+=1.999*distcloud:test=1
	If cloudx(i)>mx+distcloud Then cloudx(i)-=1.999*distcloud:test=1
	If cloudy(i)<my-distcloud Then cloudy(i)+=1.999*distcloud:test=1
	If cloudy(i)>my+distcloud Then cloudy(i)-=1.999*distcloud:test=1
	If test=1 Then
		cloudz(i)=(3000+1200*Rnd)
	EndIf
	drawcloud(i)
Next
gldisable GL_BLEND
glEnable GL_DEPTH_TEST
'gldisable gl_alpha_test
glDepthMask(GL_true)
EndIf 
'drawrain()
End Sub
'Dim Shared As Single cloudshadowdo1(ncloud),cloudshadowdo2(ncloud),cloudshadowdo3(ncloud)
'Dim Shared As Single clouddz(ncloud),testcloud(11,11)
'Dim Shared As Double timecloudshadow
'Dim Shared As Integer tupdatecloudshadow
Sub drawcloudshadow(ByVal i As Integer)
	      If tdark<>0 Then Exit sub
	      Var xx=cloudx(i)
	      Var yy=cloudy(i)
	      Var zz=max(0.0,cloudz(i))
	      xx+=zz*sunco1*suntan2*0.57
	      yy+=zz*sunsi1*suntan2*0.57
	      'if i=1 And auxtest>0.4 Then xx=mx:yy=my
	      zz=3
	      'If zz>mz-10 And mz<mzsol00+100 Then Exit Sub 
      Var size=cloudr(i)*2.5
		rotavion(xx-mx,yy-my,zz-mz)
		if x2>(0.9*max(y2,z2)-size-4000) Then
	      Var cc=0.4
	      'cc=1
      	'cc*=min(1.0,max(0.001,0.25*9000/(1000+Abs(xx-mx)+Abs(yy-my))))
      	'cc*=min(1.0,max(0.001,(mz-zz)*Abs(x2)*0.002*0.002))
	      glcolor4f(cc,cc,cc,cc)
	      'glcolor4f(1,0,0,1)
	      glpushmatrix
	      'xx=mx+1400:yy=my
	      'If xx>100-cloudr(i) Then zz=26
	      gltranslatef(xx,yy,zz)'clouddz(i)+zz+max(2.0,(mz-zz)*0.03))	      
	      'glbindtexture(gl_texture_2d,cloudtext)
         'glrotatef(cloudshadowdo1(i),0,0,1)
         glrotatef(o1,0,0,1)
	      glrotatef(-90,0,1,0)
	      'glrotatef(cloudshadowdo2(i)-90,0,1,0)
	      'glrotatef(cloudshadowdo3(i),1,0,0)
	      gltexcarre3(cloudr(i)*1.5,cloudr(i))
	      'gltexsphere(200)
	      glpopmatrix
	   EndIf    
End Sub 
Dim Shared As Integer tshadow0
Sub drawcloudshadows()
Dim As Integer i,j,k
Dim As Integer test
If tdark=1 Then Exit sub
'krain=0.1
ncloud22=ncloud2'min2(ncloud2,Int(ncloud2*krain0))
If ncloud2>0 Then 
'If mz<2500 Then Exit Sub 
'glenable gl_alpha_test
'glAlphaFunc(gl_greater,10/254)
'glenable(gl_cull_face)
'glcullface(gl_front)
'gldisable gl_fog
glenable GL_BLEND
glBlendFunc gl_zero,GL_ONE_MINUS_SRC_color
gldisable GL_DEPTH_TEST
glDepthMask(GL_false)
glenable(gl_texture_2d)
glbindtexture(gl_texture_2d,cloudtext)
Var cc=0.4'0.25
glcolor4f(cc,cc,cc,1)
For i=1 To ncloud2
	drawcloudshadow(i)
Next
glcolor4f(1,1,1,1)
glDepthMask(GL_true)
gldisable GL_BLEND
glEnable GL_DEPTH_TEST	
'gldisable gl_cull_face	
'gldisable gl_alpha_test
EndIf 
End Sub
Const As Integer ngrass=2500
Dim Shared As Single distgrass=350,waterz=-1
Dim Shared As uint grasslist,grasstext
Dim Shared As Single grassx(ngrass),grassy(ngrass),grassz(ngrass),grassrot(ngrass),grassrot0(ngrass)
Dim Shared As Single grassscalex(ngrass),grassscalez(ngrass),grasstype(ngrass),grasstx(ngrass)
Dim Shared As Single grasswind,grasswindo1,grassscalez2(ngrass)
Declare Function gettestroad(x As Single,y As Single)As Integer
'Declare Function gettestroadtree(x As Single,y As Single)As Integer
'Declare Function gettestnear0road(x As Single,y As Single,layer As Integer=0,inear0 As Integer=0)As Integer
Sub initgrass
Dim As Integer i,j  	
Dim As Integer ix,iy,x,y
Dim As Single aux
   Randomize(0)
	'distgrass=500
	Var k=15
 	For i=1 To ngrass
 	  If (i Mod (ngrass\k))=1 Then
 	  	 x=(Rnd-0.5)*distgrass
 	  	 y=(Rnd-0.5)*distgrass
 	  EndIf
	  grasstype(i)=Int(Rnd*1.19)+1	
     grassx(i)=x+(Rnd-0.5)*distgrass*0.9
     grassy(i)=y+(Rnd-0.5)*distgrass*0.9
     grassz(i)=0'getterrainheight(grassx(i),grassy(i))
     grassrot(i)=Rnd*180 
     grassscalex(i)=1+Rnd*0.15
     grassscalez(i)=(1+Rnd*0.3)*1.15'1.25
     grasstx(i)=rnd
     grassrot0(i)=grassrot(i)		
     aux=1'-terraincolor(ix,iy).x
     grassscalez2(i)=1.6*aux*grassscalez(i)
 	Next
 	Randomize()
End Sub 
Sub drawgrass
Dim As Integer i  	
Dim As Integer changegrass,ix,iy
Dim As Single x=30,y=20,tx=1,x0,x1,dtx=0.8,grasso1,aux
If grasstext=0 Then
	grasstext=guiloadtexture(ExePath+"/objects/grassobj.jpg",250)
   grasslist=loadlist(ExePath+"/objects/grassobj.3ds",7)
	initgrass()
EndIf
If mx>distgrass-250 Then Exit sub
x1=x*0.5:x0=0-x1
    If Rnd<0.15*kfps Then
    	grasswind+=(Rnd-0.5)*2.5
    	grasswind=max(-10.0,min(10.0,grasswind))
    	grasswindo1+=(Rnd-0.5)*30
    	While grasswindo1<-180:grasswindo1+=360:Wend
    	While grasswindo1>180:grasswindo1-=360:Wend
    Else
    	grasswind=(1-min(0.8,0.15*kfps))*grasswind+1e-10
    EndIf
    glcolor4f(0.65,0.9,0.65,1)
    glenable gl_alpha_test
    glAlphaFunc(gl_less,25/254)
    glbindtexture(gl_texture_2d,grasstext)	
    For i=1 To ngrass
     changegrass=0'tupdategrass	
     While grassx(i)<mx-distgrass :grassx(i)+=distgrass*2:changegrass=1:Wend 
     While grassx(i)>mx+distgrass :grassx(i)-=distgrass*2:changegrass=1:Wend 	
     While grassy(i)<my-distgrass :grassy(i)+=distgrass*2:changegrass=1:Wend 
     While grassy(i)>my+distgrass :grassy(i)-=distgrass*2:changegrass=1:Wend 
     If changegrass=1 Then
     	 If grassx(i)>-250 Then 
     	 	grassz(i)=waterz-1
     	 Else
     	 	grassz(i)=0
     	 EndIf
     	 'Var scalex=500
       'ix=Int(grassx(i)/scalex+10000)-10000
       'iy=Int(grassy(i)/scalex+10000)-10000
       'ix=max2(0,min2(512,ix))
       'iy=max2(0,min2(512,iy))
       aux=1'-terraincolor(ix,iy).x
       'grassscalez2(i)=1.6*aux*grassscalez(i)
       grassscalez2(i)=0.95*aux*grassscalez(i)
     EndIf
 If grassx(i)<-340 Then'grassz(i)>waterz  Then 
  'rotavion2(grassx(i)-mx,grassy(i)-my)
  rotavion(grassx(i)-mx,grassy(i)-my,grassz(i)-mz)
  If x2>(max(Abs(y2),Abs(z2))-80) Then 	
     glpushmatrix
     gltranslatef(grassx(i),grassy(i),grassz(i))
     glrotatef(grassrot(i),0,0,1)
     grasso1=grassrot(i)-grasswindo1+(Abs(grassx(i)-mx)+Abs(grassy(i)-my))*0.18
     While grasso1<-180:grasso1+=360:Wend
     While grasso1>180:grasso1-=360:Wend
     If grasso1>0 Then 
       glrotatef(grasswind,0,1,0)
     Else 
       glrotatef(-grasswind,0,1,0)
     EndIf
     glscalef(grassscalex(i),grassscalex(i),grassscalez2(i)) 
	/'  tx=grasstx(i)
	glbegin(gl_quads)
	glTexCoord2f(tx,0)
	glvertex3f(0,x0,0)
	gltexcoord2f(tx+dtx,0)
	glvertex3f(0,x1,0)
	glTexCoord2f(tx+dtx,1)
	glvertex3f(0,x1,y)
	gltexcoord2f(tx,1)
	glvertex3f(0,x0,y)
	glend()'/
	  glcalllist(grasslist)
     glpopmatrix
  EndIf
 EndIf  
    Next i 
    glcolor4f(1,1,1,1)
    gldisable gl_alpha_test
End Sub
Dim Shared As Double timewind
Sub drawwind'boussole
 If time1>timewind+0.15 Then
 	If timewind<0.001 Then windo1=(Rnd-0.5)*360
 	timewind=time1
 	windv=max(0.8,min(1.2,windv+(Rnd-0.5)*0.25))
 	windo1+=(Rnd-0.5)*2
 	If windo1>180 Then windo1-=360
 	If windo1<-180 Then windo1+=360
 EndIf
 glenable gl_alpha_test
 glAlphaFunc(gl_less,10/254)
 glcolor3f(1,1,1)
 glbindtexture(gl_texture_2d,windtext)
 glpushmatrix
 glloadidentity
 glplacecursor(xmax-40,40)
 glrotatef(90-o1,0,0,1)
 gltexcarre(3)
 glrotatef(windo1-90,0,0,1)
 'glcolor3f(0.7,0.8,0.7)
 gltexcarre(1.2)
 glpopmatrix
 glcolor3f(1,1,1)
 gldisable gl_alpha_test
End Sub























