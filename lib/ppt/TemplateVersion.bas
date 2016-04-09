Attribute VB_Name = "TemplateVersion"
Private Const TemplateBuildNumberPropertyName As String = "TemplateBuildNumber"

Function isTemplate() As Boolean
    On Error GoTo ErrorHandler
    Dim property As DocumentProperty
    
    Set property = ActivePresentation.CustomDocumentProperties.Item(TemplateBuildNumberPropertyName)
    isTemplate = False
    Exit Function
    
ErrorHandler:
        Select Case Err.Number
            Case 5
                isTemplate = True
                Exit Function
            Case Else
                On Error GoTo 0
        End Select
        Resume
End Function

Public Sub ShowTemplateVersion()
    Dim message As String
    If isTemplate Then
        message = "is"
    Else
        message = "presentation is based on"
    End If
    
    Call MsgBox("This " + message + " template version " + TemplateVersion, 0, "Template Version")
End Sub

Function TemplateVersion() As String
    TemplateVersion = CStr(TemplateReleaseNumber) + " (rev. " + TemplateBuildNumberProperty.value + ")"
End Function

Function RevisionNumberProperty() As DocumentProperty
    Set RevisionNumberProperty = PropertyNamed(RevisionNumberPropertyName)
End Function

Function TemplateBuildNumberProperty() As DocumentProperty
    Dim propertyName As String
    
    If isTemplate Then
        propertyName = RevisionNumberPropertyName
    Else
        propertyName = TemplateBuildNumberPropertyName
    End If
    
    Set TemplateBuildNumberProperty = PropertyNamed(propertyName)
End Function

Function PropertyNamed(aName As String) As DocumentProperty
    Dim aProperty
    
    On Error GoTo ErrorHandler
        Set PropertyNamed = ActivePresentation.BuiltInDocumentProperties.Item(index:=aName)
    Exit Function

ErrorHandler:
    Select Case Err.Number
        Case 5
            Err.Clear
            Set PropertyNamed = ActivePresentation.CustomDocumentProperties.Item(index:=aName)
            Exit Function
        Case Else
            On Error GoTo 0
    End Select
    Resume
End Function

