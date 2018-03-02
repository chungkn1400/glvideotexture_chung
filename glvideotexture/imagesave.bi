'coded by UEZ build 2017-10-05
#Include  "win/gdiplus.bi"

Using GDIPLUS

declare Function ImageSave(Image As any Ptr, Filename as WString, JPGQual as ULong = 80) as Boolean

Sub testimagesave()
	
'' Create 32-bit graphics screen and image.
Dim as ushort w = 320, h = 200, ww = 128, hh = 128
ScreenRes w, h, 32
Dim image As Any Ptr = ImageCreate( ww, hh )

Dim pitch As Integer
Dim pixels As Any Ptr

'' Get enough information to iterate through the pixel data.
If 0 <> ImageInfo( image, ,,, pitch, pixels ) Then
    Print "unable to retrieve image information."
    Sleep
    End
End If

'' Draw a pattern on the image by directly manipulating pixel memory.
For y As Integer = 0 To hh - 1
    Dim row As ulong Ptr = pixels + y * pitch
   
    For x As Integer = 0 To ww - 1
        row[x] = RGB(x * 4, y * 4, (x Xor y) * 4)
    Next x
Next y

'' Draw the image onto the screen.
Put ((w - ww) Shr 1, (h - hh) shr 1), image

ImageSave(image, "Test.bmp")
ImageSave(image, "Test.gif")
ImageSave(image, "Test.jpg")
ImageSave(image, "Test.png")
ImageSave(image, "Test.tif")

'' Destroy the image.
ImageDestroy image

Sleep 5000
End Sub 

Function ImageSave(Image As any Ptr, Filename as WString, JPGQual as ULong = 80) as Boolean
   Dim as Integer w, h, bypp, pitch
   Dim pixdata As Any Ptr
   If 0 <> ImageInfo(Image, w, h, bypp, pitch, pixdata) Then Return False
   
   Dim GDIPlusStartupInput As GDIPLUSSTARTUPINPUT
   Dim As ULONG_PTR GDIPlusToken
   GDIPlusStartupInput.GdiplusVersion = 1   
   If (GdiplusStartup(@GDIPlusToken, @GDIPlusStartupInput, NULL) <> 0) Then Return 0
   Dim as Uinteger  x, y, RowOffset
   Dim As Any Ptr hBitmap
   Dim As BitmapData tBitmapData
   Dim As Rect tRect = Type(0, 0, w - 1, h - 1)
   GdipCreateBitmapFromScan0(w, h, 0, PixelFormat32bppARGB, 0, @hBitmap)
   GdipBitmapLockBits(hBitmap, Cast(Any Ptr, @tRect), ImageLockModeWrite, PixelFormat32bppARGB, @tBitmapData)
   For y = 0 to h - 1
      RowOffset = y * w
      for x = 0 to w - 1
         Cast(Integer Ptr, tBitmapData.Scan0)[RowOffset + x] = &hFF000000 + point(x, y, Image)
      Next     
   Next
   GdipBitmapUnlockBits(hBitmap, @tBitmapData)
   
   Dim as Byte iErr = 0

   Dim as UInteger count, size
   GdipGetImageEncodersSize(@count, @size)
   Dim As ImageCodecInfo Ptr pImageCodecInfo, Clsid
   pImageCodecInfo = Allocate(size)
   GdipGetImageEncoders(count, size, pImageCodecInfo)
   
   For i as ulong = 0 to count
      If *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/bmp" and Right(Filename, 4) = ".bmp" Then
         If (GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
      Elseif *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/jpeg" and (Right(Filename, 4) = ".jpg" or Right(Filename, 5) = ".jpeg") Then
         'If (GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1   
         JPGQual = IIf(JPGQual < 0, 0, IIf(JPGQual > 100, 100, JPGQual))
         Dim tParams As EncoderParameters
         Dim EncoderQuality As String = "{1D5BE4B5-FA4A-452D-9CDD-5DB35105E7EB}"
         tParams.Count = 1
         CLSIDFromString(Wstr(EncoderQuality), @tParams.Parameter(0).GUID)
         With tParams.Parameter(0)
            .NumberOfValues = 1
            .Type = EncoderParameterValueTypeLong
            .Value = VarPtr(JPGQual)
         End With
         If GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, @tParams) <> 0 Then iErr += 1         
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/gif" and Right(Filename, 4) = ".gif" Then
        If (GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/tiff" and (Right(Filename, 4) = ".tif" or Right(Filename, 5) = ".tiff") Then
        If (GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/png" and Right(Filename, 4) = ".png" Then
        If (GdipSaveImageToFile(hBitmap, WStr(Filename), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1     
      Else
         iErr += 1
      End if
   Next

   Deallocate(pImageCodecInfo)

   GdipDisposeImage(hBitmap)
   GdiplusShutdown(GDIPlusToken)
   If iErr > 0 Then Return False

   Return True
End function
