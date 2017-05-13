Attribute VB_Name = "Filenames"
Private Function ModuleNumberFromFilename() As Integer
    Dim name As String
    Dim x, firstDigit, lastDigit
    
    ModuleNumberFromFilename = 0
    name = ActivePresentation.name
    'firstDigit = 1
    lastDigit = Len(name)
    
    ' val() doesn't like leading zeros or punctuation, so we must skip all of that
    For x = Len(name) To 1 Step -1
        ' find the last underscore
        If InStr("_", Mid(name, x, 1)) > 0 Then
            lastDigit = x
            Exit For
        End If
    Next x
    
    For x = lastDigit To 1 Step -1
        ' find the last digit before the underscore
        If InStr("1234567890", Mid(name, x, 1)) > 0 Then
            lastDigit = x
            Exit For
        End If
    Next x
    
    For x = lastDigit To 1 Step -1
        ' now find the first digit of that number
        If InStr("1234567890", Mid(name, x, 1)) = 0 Then
            firstDigit = x
            Exit For
        End If
    Next x
    
    For x = firstDigit To lastDigit Step 1
        ' skip past all leading zeros
        If InStr("123456789", Mid(name, x, 1)) > 0 Then
            ModuleNumberFromFilename = Val(Mid(name, x))
            Exit Function
        End If
    Next x
    
End Function

Private Sub RenameModuleFile()
    Dim oldName As String
    Dim newName As String
        
    oldName = ActivePresentation.name
    newName = oldName
    
    If ModuleNameChanged = True Then
        ' fetch new filename
        newName = InputBox("Rename file from '" + oldName + "' to what?" + " (Please remember to remove the spaces):", "Rename file", SuggestedFileName)
        ModuleNameChanged = False
    End If
    
    ' has the filename changed?
    If Not (newName = "") Then
        If Not (UCase(newName) = UCase(oldName)) Then
            Dim oldFullName As String
            oldFullName = ActivePresentation.FullName
         
            ' save as new file
            ActivePresentation.SaveAs ActivePresentation.Path + "\" + newName
            Call MsgBox("File has been renamed.", vbOKOnly, "File Successfully Renamed")
        
            ' delete old file
            CreateObject("Scripting.FileSystemObject").GetFile(oldFullName).Delete
        End If
     End If
     
End Sub

Private Function SuggestedFileNamePrefix()
    ' append the module number and the 3-letter code used in the previous file
    SuggestedFileNamePrefix = TwoDigitModuleNumberString + Mid$(ActivePresentation.name, 3, 3) + "_"
End Function

Private Function SuggestedFileName()
    ' append the current module number and the 3-letter code used in the previous file
    SuggestedFileName = SuggestedFileNamePrefix + ModuleName + ".ppt"
End Function


