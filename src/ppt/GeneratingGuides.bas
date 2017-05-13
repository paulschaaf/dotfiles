Attribute VB_Name = "GeneratingGuides"
' **********************************************************************
' **********************************************************************
' Generating Guides
' **********************************************************************
' **********************************************************************

'Does "generate" make ppt slide files or pps slide show files?
Public Const GeneratedSlideFormat = ppSaveAsPresentation

Public Function baseName() As String
    ' Answer the base filename; no path, no extension.
    ' Sometimes ActivePresentation.name includes the extension. Sometimes it does not.
    Dim extension As Integer
    
    baseName = ActivePresentation.name
    extension = InStr(1, baseName, ".ppt")
    If extension > 0 Then
        baseName = Mid(baseName, 1, extension - 1)
    End If
End Function

Sub GenerateStudentGuide()
    Call Generate_IG__SG_Presentations_UsingDefaults(False, True, True)
End Sub

Private Sub GenerateInstructorGuide()
    Call Generate_IG__SG_Presentations_UsingDefaults(True, False, True)
End Sub

Private Sub GenerateInstructorAndStudentGuides()
    Call Generate_IG__SG_Presentations_UsingDefaults(True, True, True)
End Sub

Sub Generate_IG__SG_Presentations_UsingDefaults(createInstructorGuides As Boolean, createStudentGuides As Boolean, createStudentPresentations As Boolean)
    Dim Course As String
    Dim backslash As String
    
    Course = ActivePresentation.Path
    While (InStr(1, Course, "\") > 0)
        backslash = InStr(1, Course, "\")
        Course = Mid(Course, backslash + 1, Len(Course))
    Wend
    
    Call GenerateGuidesUsing(Course, ActivePresentation.Path, createInstructorGuides, createStudentGuides, createStudentPresentations)
End Sub

Sub GenerateGuidesUsing(Course As String, destDir As String, createInstructorGuides As Boolean, createStudentGuides As Boolean, createStudentPresentations As Boolean)
'
    Dim baseFileName As String
    Dim igName As String
    
    baseFileName = "_" + Course + "_" + baseName()
    
    SetReleaseMode
        
    'create internal ("confidential") version at this point
    
    ' Don't bother to make IG versions if they will be indistinct from the SG version
    If (createInstructorGuides And HasDistinctInstructorGuideVersion) Then
        ViewInstructorGuide
        igName = destDir + "\IG" + baseFileName
        ActivePresentation.SaveAs igName
        ActivePresentation.PrintOut
    End If

    If createStudentGuides Or createStudentPresentations Then
        ViewStudentGuide
        
        If createStudentPresentations Then
            Dim newName As String
            newName = destDir + "\SG" + baseFileName
            ActivePresentation.SaveAs newName, GeneratedSlideFormat
        End If
        
        If createStudentGuides Then
            ActivePresentation.PrintOut
        End If
    End If
    
    ActivePresentation.Close
End Sub

Public Function HasAnyInstructorNotes() As Boolean
    If (Len(AllInstructorNotes()) = 0) Then
        HasAnyInstructorNotes = False
    Else
        HasAnyInstructorNotes = True
    End If
End Function

Public Function HasDistinctInstructorGuideVersion() As Boolean
    HasDistinctInstructorGuideVersion = HasAnyInstructorNotes
End Function

'Private Sub PrintHandouts()
'    ViewDeveloperGuide
'
'    SetupPortraitPage
'    With ActivePresentation.PrintOptions
'        .FrameSlides = True
'        .OutputType = ppPrintOutputTwoSlideHandouts
'    End With
'
'    'bring up the print dialog
'    Application.CommandBars("File").Controls("&Print...").Execute
'
'    MsgBox "After closing the Print dialog, hit ok to refresh the view."
'    ViewDeveloperGuide
'End Sub

Private Sub RemoveSlides(coll As Collection)
    Dim aSlide As Slide
    
    For Each aSlide In coll
        aSlide.Delete
    Next aSlide
End Sub

