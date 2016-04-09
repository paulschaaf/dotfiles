Attribute VB_Name = "Properties"
Public Const ModuleNamePropertyName As String = "Title"
Public Const RevisionNumberPropertyName As String = "Revision number"

Dim ModuleNameChanged As Boolean

Public Sub SetModuleVariables()
'
' Prompts user for new values of module variables like Module name and number.
'
    'SetSectionName
    'SetSectionVersionNumber
    SetModuleName
    ViewDeveloperGuide
        
    'If (ModuleNameChanged = True) Then
    '    RenameModuleFile
    'End If

End Sub

Private Sub SetModuleName()
    SetModuleNameTo GetModuleNameFromUser
End Sub

Private Function GetModuleNameFromUser() As String
    GetModuleNameFromUser = InputBox("Change module name" + Chr(13) + Chr(13) + Chr(9) + ModuleName + Chr(13) + Chr(13) + "to:", "Set Module Name")
End Function

Private Sub SetModuleNameTo(newName As String)
    ModuleNameChanged = Not ((newName = "") Or (newName = ModuleName))
    
    If ModuleNameChanged Then
        PropertyNamed(ModuleNamePropertyName).value = newName
    End If
End Sub

Function ModuleName() As String
    ModuleName = PropertyNamed(ModuleNamePropertyName).value
End Function

Function ModuleNameFormalString() As String
    ModuleNameFormalString = ModuleNumberFormalString + ": " + ModuleName
End Function

Public Function ModuleNameBox() As Shape
    Set ModuleNameBox = ThisSlideMaster.shapes(ModuleNameBoxName)
End Function

Function ModuleNumber() As Integer
    ModuleNumber = ModuleNumberFromFilename
End Function

Function ModuleNumberString() As String
'
' Answer a string version of the module number.
'
    Dim aNumber
    
    aNumber = ModuleNumber
    If aNumber = 0 Then
        ModuleNumberString = "i"
    Else
        ModuleNumberString = CStr(aNumber)
    End If
End Function

Function TwoDigitModuleNumberString() As String
    Dim aNumber
    
    aNumber = ModuleNumber
    If aNumber < 10 Then
        TwoDigitModuleNumberString = "0" + CStr(aNumber)
    Else
        TwoDigitModuleNumberString = CStr(aNumber)
    End If
End Function

Function ModuleNumberFormalString() As String
    ModuleNumberFormalString = "Module " + ModuleNumberString
End Function


