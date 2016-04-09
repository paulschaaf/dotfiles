Attribute VB_Name = "Porting"
' **********************************************************************
' **********************************************************************
' Porting Files
' **********************************************************************
' **********************************************************************

' todo: make these document properties

' **********************************************************************
' Porting file directories
Public Const WorkingDirectory = "c:\MyDocuments\courses"
Public Const DevelopmentDirectory = WorkingDirectory + "\sections"
Public Const PortingSourceDirectory = DevelopmentDirectory
Public Const PortingDestinationDirectory = DevelopmentDirectory


Public Function BooleanMsgBox(aMessage As String, aTitle As String) As Boolean
    If (MsgBox(aMessage, vbYesNo, aTitle)) = vbYes Then
        BooleanMsgBox = True
    Else
        BooleanMsgBox = False
    End If
End Function

Function KeywordsProperty() As DocumentProperty
    Set KeywordsProperty = PropertyNamed("Keywords")
End Function

Private Function GetKeywordsFromUser() As String
    GetKeywordsFromUser = InputBox("Please type the keywords to be added to all files (existing keywords will be preserved). Separate each word with a space.", "Add Module Keywords", "")
End Function

Private Sub SetTemplateBuildNumberTo(aVersion As String)
    Dim props As DocumentProperties
    
    Set props = ActivePresentation.CustomDocumentProperties
    With props
        .Add name:=TemplateBuildNumberPropertyName, LinkToContent:=False, Type:=msoPropertyTypeString, value:=aVersion
    End With
    RevisionNumberProperty.value = 0
End Sub

Sub PortFiles()
    Dim sourceDir As String
    Dim destDir As String
    Dim aSectionName As String
    Dim aSectionVersionNumber As String
    Dim keywords As String
    Dim excludeTemplateSlides
    Dim preservePatchNumber
    
    sourceDir = InputBox("What is the source directory?", "Source Directory?", PortingSourceDirectory)
    destDir = InputBox("What is the destination directory?", "Destination Directory?", PortingDestinationDirectory)
    
    aSectionName = GetSectionNameFromUser
    aSectionVersionNumber = GetSectionVersionNumberFromUser
    keywords = GetKeywordsFromUser
    
    ' should we include slides from template?
    If (ActivePresentation.Slides.count > 0) Then
        excludeTemplateSlides = BooleanMsgBox("Exclude template slides from ported files?", "Exclude slides?")
    Else
        excludeTemplateSlides = True
    End If
    
    'should we clear the patch number?
    preservePatchNumber = BooleanMsgBox("Try to preserve patch number ('No' will reset to zero)?", "Preserve patch number?")
    
    If Not BooleanMsgBox("Cancel porting of files?", "Cancel?") Then
        PortFilesUsing sourceDir, destDir, aSectionName, aSectionVersionNumber, keywords, excludeTemplateSlides, preservePatchNumber
    End If
    
End Sub

Private Sub PortFilesUsingDefaults()
    PortFilesFrom PortingSourceDirectory
End Sub

Private Sub QuickPortFiles()
    Dim sourceDir As String
    sourceDir = InputBox("What is the source directory?", "Source Directory?", PortingSourceDirectory)
    PortFilesFromTo sourceDir, PortingDestinationDirectory
End Sub

Sub PortFilesFrom(sourceDir As String)
    PortFilesFromTo sourceDir, PortingDestinationDirectory
End Sub

Sub PortFilesFromTo(sourceDir As String, destDir As String)
    PortFilesFromToSectionNameNumberKeywords sourceDir, destDir, "", "", ""
End Sub

Sub PortFilesFromToSectionNameNumberKeywords(sourceDir As String, destDir As String, aSectionName As String, aSectionVersionNumber As String, keywords As String)
    PortFilesUsing sourceDir, destDir, aSectionName, aSectionVersionNumber, keywords, True, True
End Sub

Sub PortFilesUsing(sourceDir As String, destDir As String, aSectionName As String, aSectionVersionNumber As String, keywords As String, excludeTemplateSlides, preservePatchNumber)
'
' Must be run from the template! Creates new presentations using this template for each
' presentation in the source directory.
'
    Dim oldPres As Presentation
    Dim newPres As Presentation
    Dim fileName As String
    Dim thisTemplate As String
    Dim aVersionNumber As String
    
    If Not (isTemplate) Then
        MsgBox ("Error: This macro must be run from the template, not from a presentation based upon the template.")
        Exit Sub
    End If
    
    On Error Resume Next
    
    thisTemplate = ActivePresentation.FullName
    aVersionNumber = RevisionNumberProperty.value
    
    'for each presentation in the source directory
    fileName = Dir(sourceDir + "\*.ppt")
    Do While fileName <> ""
        ' Open old presentation, copy all slides and notes
        Set oldPres = Application.Presentations.Open(sourceDir + "\" + fileName, True)
        oldPres.Slides.Range.Copy
    
        'create a new presentation based upon this tempate, emptying it afterward if specified
        ' Paste slides and notes into new presentation
        Set newPres = Presentations.Open(thisTemplate, False)
        If (excludeTemplateSlides) Then
            newPres.Slides.Range.Delete
            newPres.Slides.Paste
        Else
            newPres.Slides.Paste (3)
        End If
        
        ' format new presentation
        FormatAllInstructorNotes
        
        ' copy document properties
        Call CopyPropertiesFromTo(oldPres, newPres)
        SetTemplateBuildNumberTo (aVersionNumber)
        If Len(aSectionName) > 0 Then
            SetSectionNameTo aSectionName
        End If
        If Len(aSectionVersionNumber) > 0 Then
            SetSectionVersionNumberTo (aSectionVersionNumber)
        End If
        
        KeywordsProperty.value = Trim(KeywordsProperty.value + " " + keywords)
        
        If Not preservePatchNumber Then
            RevisionNumberProperty.value = 0
        End If
        
        ' Close presentations
        oldPres.Close
        
        newPres.SaveAs destDir + "\" + fileName
        Application.Run newPres.name + "!ViewDeveloperGuide"
        newPres.Save
        
        newPres.Close
        
        ' Increment loop counter
        fileName = Dir
    Loop
End Sub

Private Sub CopyPropertiesFromTo(source As Presentation, dest As Presentation)
    Dim sourceProps As DocumentProperties
    Dim destProps As DocumentProperties
    
    Set sourceProps = source.BuiltInDocumentProperties
    Set destProps = dest.BuiltInDocumentProperties
    
    CopyPropertyFromTo "Title", sourceProps, destProps
    CopyPropertyFromTo "Subject", sourceProps, destProps
    CopyPropertyFromTo "Author", sourceProps, destProps
    CopyPropertyFromTo "Manager", sourceProps, destProps
    destProps("Company").value = "Versant Corporation"
    CopyPropertyFromTo "Keywords", sourceProps, destProps
    CopyPropertyFromTo "Comments", sourceProps, destProps
    
    Set sourceProps = source.CustomDocumentProperties
    Set destProps = dest.CustomDocumentProperties
End Sub

Private Sub CopyPropertyFromTo(aString As String, oldProps As DocumentProperties, newProps As DocumentProperties)
    On Error Resume Next
    newProps(aString).value = oldProps(aString).value
End Sub

