Attribute VB_Name = "Section"
Public Const SectionNamePropertyName As String = "Subject"
Public Const SectionVersionNumberPropertyName As String = "SectionVersionNumber"
Public Const SectionNameBoxName As String = "SectionName"

Public Sub ShowSectionVersion()
    Dim sectionVer As String
    Dim message As String
    
    If isTemplate Then
        message = "This is template version " + TemplateVersion
        Call MsgBox(message, 0, "Template Version")
    Else
        sectionVer = "This module is from section version " + SectionVersionNumber() + " patch " + RevisionNumberProperty.value + "."
        message = " It is based on the PPT template version " + TemplateVersion() + "."
        Call MsgBox(sectionVer + message, 0, "Section Version")
    End If
End Sub

Function SectionName() As String
    SectionName = SectionNameProperty.value
End Function

Function SectionNameProperty() As DocumentProperty
    Set SectionNameProperty = PropertyNamed(SectionNamePropertyName)
End Function

Function SectionPublicVersionNumber() As String
    Dim aNumber, firstDot, secondDot
    
    aNumber = SectionVersionNumber
    If isTemplate Then
        SectionPublicVersionNumber = aNumber
    Else
        firstDot = InStr(aNumber, ".") + 1
        secondDot = InStr(firstDot, aNumber, ".") - 1
        SectionPublicVersionNumber = Mid(aNumber, 1, secondDot)
    End If
End Function

Function SectionVersionNumber() As String
    On Error GoTo ErrorHandler
    If isTemplate Then
        SectionVersionNumber = TemplateVersion
    Else
        SectionVersionNumber = SectionVersionNumberProperty.value
    End If
    Exit Function
    
ErrorHandler:
    SectionVersionNumber = "x.x.x"
End Function

Private Sub SetSectionVersionNumber()
    SetSectionVersionNumberTo GetSectionVersionNumberFromUser
End Sub

Private Sub SetSectionName()
    SetSectionNameTo GetSectionNameFromUser
End Sub

Private Function GetSectionNameFromUser() As String
    GetSectionNameFromUser = InputBox("Change section name" + Chr(13) + Chr(13) + Chr(9) + SectionName + Chr(13) + Chr(13) + "to:", "Set Section Name", "")
End Function

Private Function GetSectionVersionNumberFromUser() As String
    GetSectionVersionNumberFromUser = InputBox("Change section version number (usually xx.yy.zz)" + Chr(13) + Chr(13) + Chr(9) + SectionVersionNumber + Chr(13) + Chr(13) + "to:", "Set Section Version")
End Function

Private Sub SetSectionNameTo(newName As String)
    Dim sectionNameChanged As Boolean
    'section name unchanged if newName is empty or matches current name
    sectionNameChanged = Not ((newName = "") Or (newName = SectionName))

    If sectionNameChanged Then
        SectionNameProperty.value = newName
        If isSection Then
            SetModuleNameTo newName
        End If
    End If

End Sub

Private Sub SetSectionVersionNumberTo(aVersion As String)
    PropertyNamed(SectionVersionNumberPropertyName).value = aVersion
End Sub

Public Function SectionNameBox() As Shape
    Set SectionNameBox = ThisNotesMaster.shapes(SectionNameBoxName)
End Function

Public Function isSection() As Boolean
    isSection = (ModuleNumber() = 0)
End Function

Function SectionVersionNumberProperty() As DocumentProperty
    Set SectionVersionNumberProperty = PropertyNamed(SectionVersionNumberPropertyName)
End Function

