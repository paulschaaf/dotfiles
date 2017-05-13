Attribute VB_Name = "Functions"
' Module written by Paul Schaaf
' paul.schaaf@gmail.com

' Require declaration of all variables
Option Explicit

' **********************************************************************
' **********************************************************************
' User modifiable

Public Const PDFPrinter = "PDFCreator"

' **********************************************************************
' Page Layout -- Portrait

Public Const Gutter_Portrait = 10
Public Const PrintableHeight_Portrait = 720
Public Const VPadding_Portrait = 10

Public Const OverheadHeight_Portrait = 274.125
Public Const OverheadWidth_Portrait = 365.5
Public Const OverheadTop_Portrait = 54.875
Public Const OverheadLeft_Portrait = 92.75

Public Const StudentNoteTop_Portrait = OverheadTop_Portrait + OverheadHeight_Portrait + VPadding_Portrait
Public Const StudentNoteHeight_Portrait = PrintableHeight_Portrait - StudentNoteTop_Portrait
Public Const StudentNoteWidth_Portrait = OverheadWidth_Portrait
Public Const StudentNoteLeft_Portrait = OverheadLeft_Portrait

Public Const InstructorNoteTop_Portrait = StudentNoteTop_Portrait + (StudentNoteHeight_Portrait / 2)
Public Const InstructorNoteHeight_Portrait = PrintableHeight_Portrait - InstructorNoteTop_Portrait
Public Const InstructorNoteWidth_Portrait = OverheadWidth_Portrait
Public Const InstructorNoteLeft_Portrait = OverheadLeft_Portrait


' **********************************************************************
' Page Layout -- Landscape

Public Const Gutter_Landscape = 10
Public Const PrintableHeight_Landscape = 490
Public Const VPadding_Landscape = Gutter_Landscape

Public Const OverheadHeight_Landscape = 252.125
Public Const OverheadWidth_Landscape = 340
Public Const OverheadTop_Landscape = 41.625
Public Const OverheadLeft_Landscape = 23.5

Public Const StudentNoteHeight_Landscape = PrintableHeight_Landscape - OverheadHeight_Landscape - VPadding_Landscape
Public Const StudentNoteWidth_Landscape = OverheadWidth_Landscape
Public Const StudentNoteTop_Landscape = OverheadTop_Landscape + OverheadHeight_Landscape + VPadding_Landscape
Public Const StudentNoteLeft_Landscape = OverheadLeft_Landscape

Public Const InstructorNoteHeight_Landscape = PrintableHeight_Landscape
Public Const InstructorNoteWidth_Landscape = OverheadWidth_Landscape
Public Const InstructorNoteTop_Landscape = OverheadTop_Landscape
Public Const InstructorNoteLeft_Landscape = OverheadLeft_Landscape + OverheadWidth_Landscape + Gutter_Landscape


' todo: make these document properties

' **********************************************************************
' Porting file directories
Public Const WorkingDirectory = "c:\MyDocuments\courses"
Public Const DevelopmentDirectory = WorkingDirectory + "\sections"
Public Const PortingSourceDirectory = DevelopmentDirectory
Public Const PortingDestinationDirectory = DevelopmentDirectory

' **********************************************************************
' **********************************************************************
' **********************************************************************
' NOT USER MODIFIABLE

' Used as references to these items
Public Const RevisionNumberPropertyName As String = "Revision number"
Public Const SectionNamePropertyName As String = "Subject"
Public Const SectionVersionNumberPropertyName As String = "SectionVersionNumber"
Public Const ModuleNamePropertyName As String = "Title"
Public Const TemplateBuildNumberPropertyName As String = "TemplateBuildNumber"

Public Const OverheadNumber As Integer = 1
Public Const StudentNoteNumber As Integer = 2
Public Const InstructorNoteMasterNumber As Integer = 8

'Does "generate" make ppt slide files or pps slide show files?
Public Const GeneratedSlideFormat = ppSaveAsPresentation

Public Const OverheadName As String = "Overhead"
Public Const InstructorNoteName As String = "InstructorNote"
Public Const StudentNoteName As String = "StudentNote"
Public Const ModuleNameBoxName As String = "ModuleNameBox"
Public Const PageNumberBox As String = "PageNumberBox"
Public Const SectionNameBoxName As String = "SectionName"

'Only the template developer should modify this value
Public Const TemplateReleaseNumber = 1.1

Dim developerMode As Boolean
Dim StudentGuideMode As Boolean

Dim ModuleNameChanged As Boolean

' **********************************************************************
' **********************************************************************
' Utility
' **********************************************************************
' **********************************************************************

Public Function BooleanMsgBox(aMessage As String, aTitle As String) As Boolean
    If (MsgBox(aMessage, vbYesNo, aTitle)) = vbYes Then
        BooleanMsgBox = True
    Else
        BooleanMsgBox = False
    End If
End Function

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

Public Function isOdd(anInteger As Integer) As Boolean
    If (anInteger And 1) = 1 Then
        isOdd = True
    Else
        isOdd = False
    End If
End Function

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


' **********************************************************************
' **********************************************************************
' Generating Guides
' **********************************************************************
' **********************************************************************

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
    RemoveSlides SlidesExcludingCourse(Course)
        
    'create internal ("confidential") version at this point
    
    RemoveSlides AllConfidentialSlides()

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


' **********************************************************************
' **********************************************************************
'
' **********************************************************************
' **********************************************************************

Private Sub SetPrintOptions()
    On Error Resume Next
    ActivePresentation.PrintOptions.ActivePrinter = PDFPrinter
    
    With ActivePresentation.PrintOptions
        .FrameSlides = False
        .OutputType = ppPrintOutputNotesPages
        .RangeType = ppPrintAll
        .NumberOfCopies = 1
        .Collate = msoTrue
        .FitToPage = msoFalse
    End With
    SetupPage
End Sub

Public Function OverheadOn(aSlide As Slide) As Shape
'
' Select the overhead portion of the slide.
'
    Set OverheadOn = ElementOnNotesPageWithIdentifier(aSlide, OverheadNumber)
End Function

Public Function StudentNoteOn(aSlide As Slide) As Shape
'
' Answer the student note on the supplied slide.
'
    Set StudentNoteOn = ElementOnNotesPageWithIdentifier(aSlide, StudentNoteNumber)
End Function

Public Function HasDistinctInstructorGuideVersion() As Boolean
    HasDistinctInstructorGuideVersion = HasAnyInstructorNotes
End Function

Public Function HasAnyInstructorNotes() As Boolean
    If (Len(AllInstructorNotes()) = 0) Then
        HasAnyInstructorNotes = False
    Else
        HasAnyInstructorNotes = True
    End If
End Function

Public Function HasInstructorNote(aSlide As Slide) As Boolean
'
' Answer whether the supplied slide has an instructor note.
'
    Dim iNote As Shape
    
    On Error GoTo NoNoteFound
    
    HasInstructorNote = True
    Set iNote = ElementOnNotesPageWithIdentifier(aSlide, InstructorNoteName)
    Exit Function
    
NoNoteFound:
    HasInstructorNote = False
End Function

Public Function InstructorNoteOn(aSlide As Slide) As Shape
'
' Answer the instructor note on the supplied slide.
'
    Set InstructorNoteOn = ElementOnNotesPageWithIdentifier(aSlide, InstructorNoteName)
End Function

Public Sub ShowSpecialMarkups()
'
' Shows which courses the current slide is excluded from.
'
    Dim Exclusions, message
    
    Exclusions = CourseExclusionsOn(ThisSlide)
    If Len(Exclusions) = 0 Then
        message = "This slide has no exclusions."
    Else
        message = "This slide excluded from courses: " + Exclusions
    End If
    
    If HasInstructorNote(ThisNotesPage) Then
        message = message + " It has an Instructor Note."
    Else
        message = message + " It does not have an Instructor Note."
    End If
    
    If IsConfidential(ThisNotesPage) Then
        message = message + " It is marked CONFIDENTIAL."
    Else
        message = message + " It is not marked confidential."
    End If
    
    Call MsgBox(message, 0, "Course Exclusions")
End Sub

Public Sub ShowAllSpecialMarkups()
'
' Displays a summary dialog of all excluded slides.
'
    Dim message
    
    message = AllCourseExclusions
    If Len(message) = 0 Then
        message = "There are no excluded slides."
    Else
        message = "The following slides have exclusions: " + message
    End If
    Call MsgBox(message, 0, "All Course Exclusions")
    
    message = AllInstructorNotes
    If Len(message) = 0 Then
        message = "There are no Instructor Notes."
    Else
        message = "The following slides have Instructor Notes: " + message
    End If
    Call MsgBox(message, 0, "All Instructor Notes")
    
    message = AllConfidentialSlideNumbers
    If Len(message) = 0 Then
        message = "There are no confidential slides."
    Else
        message = "The following slides are confidential: " + message
    End If
    
    Call MsgBox(message, 0, "All Confidential Slides")
End Sub

Public Function AllInstructorNotes() As String
    Dim message As String
    Dim iNote As Shape
    Dim aSlide As Slide
    
    For Each aSlide In ActivePresentation.Slides
        If (HasInstructorNote(aSlide)) Then
            If Len(message) > 0 Then
                message = message + ", "
            End If
            message = message + Str(aSlide.slideNumber)
        End If
    Next aSlide
    
    AllInstructorNotes = message
    
End Function

Public Function ModuleNameBox() As Shape
    Set ModuleNameBox = ThisSlideMaster.shapes(ModuleNameBoxName)
End Function

Public Function SectionNameBox() As Shape
    Set SectionNameBox = ThisNotesMaster.shapes(SectionNameBoxName)
End Function

Public Function MasterOverhead() As Shape
    Set MasterOverhead = ThisNotesMaster.shapes(OverheadName)
End Function

Public Function MasterStudentNote() As Shape
    Set MasterStudentNote = ThisNotesMaster.shapes(StudentNoteName)
End Function

Public Function MasterInstructorNote() As Shape
    Set MasterInstructorNote = ThisNotesMaster.shapes(InstructorNoteName)
End Function

Public Function NotesPageFor(aSlide As Slide) As Slide
'
' Answer the notespage for this slide.
'
    On Error Resume Next
    Set NotesPageFor = aSlide
    Set NotesPageFor = NotesPageFor.Parent
    Set NotesPageFor = NotesPageFor.Parent
End Function


Public Function NotesPageShapesOn(aSlide As Slide) As shapes
    Set NotesPageShapesOn = aSlide.notespage(1).shapes
End Function

Public Function ElementOnNotesPageWithIdentifier(aSlide As Slide, id) As Shape
    Set ElementOnNotesPageWithIdentifier = NotesPageShapesOn(aSlide)(id)
End Function

Public Function ElementOnSlideWithIdentifier(aSlide As Slide, id) As Shape
    Set ElementOnSlideWithIdentifier = aSlide.shapes(id)
End Function

Function ElementOnSlideMasterWithIdentifier(id) As Shape
    Set ElementOnSlideMasterWithIdentifier = ThisSlideMaster.shapes(id)
End Function

Function ElementOnTitleMasterWithIdentifier(id) As Shape
    Set ElementOnTitleMasterWithIdentifier = ThisTitleMaster.shapes(id)
End Function

Private Sub ShowOrHideDevelopmentCues(developerMode As Boolean)
    ActivePresentation.DisplayComments = developerMode
    MasterCourseExclusionsLabel.Visible = developerMode
End Sub

Sub CreateOrDeleteFinalBlankPage(developerMode As Boolean)
    Dim count As Integer
    Dim aSlide As Slide
    
    count = ActivePresentation.Slides.count
    If (developerMode) Then
        ' remove empty final slide if one exists
        Set aSlide = ActivePresentation.Slides(count)
        If (aSlide.Layout = ppLayoutBlank) Then
            aSlide.Delete
        End If
    Else
        'if necessary adds a page with no slide to make an even count for printing
        If isOdd(count) Then
            ActivePresentation.Slides.Add count + 1, ppLayoutBlank
            OverheadOn(ActivePresentation.Slides(count + 1)).Visible = msoFalse
            UpdatePageCount
        End If
    End If
End Sub

Private Sub PrintHandouts()
    ViewDeveloperGuide
    
    SetupPortraitPage
    With ActivePresentation.PrintOptions
        .FrameSlides = True
        .OutputType = ppPrintOutputTwoSlideHandouts
    End With
    
    'bring up the print dialog
    Application.CommandBars("File").Controls("&Print...").Execute
    
    MsgBox "After closing the Print dialog, hit ok to refresh the view."
    ViewDeveloperGuide
End Sub

Private Sub RemoveSlides(coll As Collection)
    Dim aSlide As Slide
    
    For Each aSlide In coll
        aSlide.Delete
    Next aSlide
End Sub

Private Sub PositionShapeFromPrototype(aShape As Shape, aPrototype As Shape)
    With aShape
        .height = aPrototype.height
        .width = aPrototype.width
        .top = aPrototype.top
        .left = aPrototype.left
    End With
End Sub

Sub PositionMasterElements()
    If (UsingLandscape) Then
        With MasterInstructorNote
            .height = InstructorNoteHeight_Landscape
            .width = InstructorNoteWidth_Landscape
            .top = InstructorNoteTop_Landscape
            .left = InstructorNoteLeft_Landscape
        End With
        With MasterOverhead
            .height = OverheadHeight_Landscape
            .width = OverheadWidth_Landscape
            .top = OverheadTop_Landscape
            .left = OverheadLeft_Landscape
        End With
        With MasterStudentNote
            .height = StudentNoteHeight_Landscape
            .width = StudentNoteWidth_Landscape
            .top = StudentNoteTop_Landscape
            .left = StudentNoteLeft_Landscape
        End With
    Else
        With MasterInstructorNote
            .height = InstructorNoteHeight_Portrait
            .width = InstructorNoteWidth_Portrait
            .top = InstructorNoteTop_Portrait
            .left = InstructorNoteLeft_Portrait
        End With
        With MasterOverhead
            .height = OverheadHeight_Portrait
            .width = OverheadWidth_Portrait
            .top = OverheadTop_Portrait
            .left = OverheadLeft_Portrait
        End With
        With MasterStudentNote
            .height = StudentNoteHeight_Portrait
            .width = StudentNoteWidth_Portrait
            .top = StudentNoteTop_Portrait
            .left = StudentNoteLeft_Portrait
        End With
    End If
End Sub

Private Sub SetupPage()
    With ActivePresentation.PageSetup
        .SlideSize = ppSlideSizeOnScreen
        .FirstSlideNumber = 1
        .SlideOrientation = msoOrientationHorizontal
    End With
    
    If (StudentGuideMode) Then
        'portrait mode
        ActivePresentation.PageSetup.NotesOrientation = msoOrientationVertical
    Else
        'landscape mode
        ActivePresentation.PageSetup.NotesOrientation = msoOrientationHorizontal
    End If
End Sub

Public Function UsingLandscape() As Boolean
    UsingLandscape = ActivePresentation.PageSetup.NotesOrientation = msoOrientationHorizontal
End Function

Sub AddInstructorNote()
'
' Adds an instructor note section to the current slide (if it doesn't already have one). These notes
' will not show up in the student version of the book.
'
    ViewNotesPage
    AddInstructorNoteTo ThisNotesPage
    InstructorNoteOn(ThisNotesPage).GroupItems(2).TextFrame.TextRange.Select
End Sub

Sub AddInstructorNoteTo(aSlide As Slide)
'
' Adds a section for instructor notes, unless one already exists.
'
    Dim aNewNote As Shape
    
    If (HasInstructorNote(aSlide)) Then Exit Sub
    
    MasterInstructorNote.Copy
    ActiveWindow.View.Paste
    With ActiveWindow.Selection.ShapeRange(1)
        .name = InstructorNoteName
        .Visible = True
    End With
        
End Sub

Public Function isSection() As Boolean
    isSection = (ModuleNumber() = 0)
End Function

Private Sub UpdatePageCount()
    With ThisSlideMaster.shapes(PageNumberBox).TextFrame.TextRange
        .InsertSlideNumber
        .InsertAfter (" of ")
        .InsertAfter (CStr(ActivePresentation.Slides.count))
    End With
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
    
    CopyPropertyFromTo SectionVersionNumberPropertyName, sourceProps, destProps
End Sub

Private Sub CopyPropertyFromTo(aString As String, oldProps As DocumentProperties, newProps As DocumentProperties)
    On Error Resume Next
    newProps(aString).value = oldProps(aString).value
End Sub
    
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

Function RevisionNumberProperty() As DocumentProperty
    Set RevisionNumberProperty = PropertyNamed(RevisionNumberPropertyName)
End Function

Function SectionNameProperty() As DocumentProperty
    Set SectionNameProperty = PropertyNamed(SectionNamePropertyName)
End Function

Function KeywordsProperty() As DocumentProperty
    Set KeywordsProperty = PropertyNamed("Keywords")
End Function

Function TemplateVersion() As String
    TemplateVersion = CStr(TemplateReleaseNumber) + " (rev. " + TemplateBuildNumberProperty.value + ")"
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

Function TemplateBuildNumberProperty() As DocumentProperty
    Dim propertyName As String
    
    If isTemplate Then
        propertyName = RevisionNumberPropertyName
    Else
        propertyName = TemplateBuildNumberPropertyName
    End If
    
    Set TemplateBuildNumberProperty = PropertyNamed(propertyName)
End Function

Function SectionVersionNumberProperty() As DocumentProperty
    Set SectionVersionNumberProperty = PropertyNamed(SectionVersionNumberPropertyName)
End Function

Function ModuleName() As String
    ModuleName = PropertyNamed(ModuleNamePropertyName).value
End Function

Function ModuleNumber() As Integer
    ModuleNumber = ModuleNumberFromFilename
End Function

Public Sub SetModuleVariables()
'
' Prompts user for new values of module variables like Module name and number.
'
    SetSectionName
    SetSectionVersionNumber
    SetModuleName
    ViewDeveloperGuide
        
    'If (ModuleNameChanged = True) Then
    '    RenameModuleFile
    'End If

End Sub

Private Sub SetSectionVersionNumber()
    SetSectionVersionNumberTo GetSectionVersionNumberFromUser
End Sub

Private Sub SetSectionName()
    SetSectionNameTo GetSectionNameFromUser
End Sub

Private Sub SetModuleName()
    SetModuleNameTo GetModuleNameFromUser
End Sub

Private Function GetSectionNameFromUser() As String
    GetSectionNameFromUser = InputBox("Change section name" + Chr(13) + Chr(13) + Chr(9) + SectionName + Chr(13) + Chr(13) + "to:", "Set Section Name", "")
End Function

Private Function GetSectionVersionNumberFromUser() As String
    GetSectionVersionNumberFromUser = InputBox("Change section version number (usually xx.yy.zz)" + Chr(13) + Chr(13) + Chr(9) + SectionVersionNumber + Chr(13) + Chr(13) + "to:", "Set Section Version")
End Function

Private Function GetModuleNameFromUser() As String
    GetModuleNameFromUser = InputBox("Change module name" + Chr(13) + Chr(13) + Chr(9) + ModuleName + Chr(13) + Chr(13) + "to:", "Set Module Name")
End Function

Private Function GetKeywordsFromUser() As String
    GetKeywordsFromUser = InputBox("Please type the keywords to be added to all files (existing keywords will be preserved). Separate each word with a space.", "Add Module Keywords", "")
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

Private Sub SetTemplateBuildNumberTo(aVersion As String)
    Dim props As DocumentProperties
    
    Set props = ActivePresentation.CustomDocumentProperties
    With props
        .Add name:=TemplateBuildNumberPropertyName, LinkToContent:=False, Type:=msoPropertyTypeString, value:=aVersion
    End With
    RevisionNumberProperty.value = 0
End Sub

Private Sub SetSectionVersionNumberTo(aVersion As String)
    PropertyNamed(SectionVersionNumberPropertyName).value = aVersion
End Sub

Private Sub SetModuleNameTo(newName As String)
    ModuleNameChanged = Not ((newName = "") Or (newName = ModuleName))
    
    If ModuleNameChanged Then
        PropertyNamed(ModuleNamePropertyName).value = newName
    End If
End Sub

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

Function ModuleNameFormalString() As String
    ModuleNameFormalString = ModuleNumberFormalString + ": " + ModuleName
End Function

Function ModuleNumberFormalString() As String
    ModuleNumberFormalString = "Module " + ModuleNumberString
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

Private Function TitleSlide() As Slide
    Dim aSlide As Slide
    
    For Each aSlide In ActivePresentation.Slides
        If aSlide.Layout = ppLayoutTitle Then
            Set TitleSlide = aSlide
            Exit Function
        End If
    Next aSlide
End Function

Private Function IndexOfSummarySlide() As Long
    IndexOfSummarySlide = IndexOfSlideWithTitle(ModuleSummarySlideTitle)
End Function

Private Function IndexOfObjectivesSlide() As Long
    IndexOfObjectivesSlide = IndexOfSlideWithTitle(ModuleObjectivesSlideTitle)
End Function

Private Function IndexOfSlideWithTitle(aTitle As String) As Long

    Dim aSlide As Slide
    Dim counter As Long
    Dim slideTitle As String
    
    On Error Resume Next
    
    slideTitle = ""
    counter = 0
    For Each aSlide In ActivePresentation.Slides
        counter = counter + 1
        slideTitle = aSlide.shapes.Title.TextFrame.TextRange.text
        If slideTitle = aTitle Then
            IndexOfSlideWithTitle = counter
            Exit Function
        End If
    Next aSlide
    
    IndexOfSlideWithTitle = 0
    
End Function

Function Split(target As String, subString As String) As Variant
    Dim startIdx
    Dim firstHalf As String
    Dim secondHalf As String
    Dim answer As Variant
    
    startIdx = InStr(target, subString)
    firstHalf = Mid(target, 1, startIdx)
    secondHalf = Mid(target, startIdx + Len(subString), Len(target))
    answer = Array(firstHalf, secondHalf)
    Split = answer

End Function

Function SlideBodyShape(aSlide As Slide) As Shape

    Dim aShape As Shape
    
    On Error Resume Next
    
    For Each aShape In aSlide.shapes
        ' find the first placeholder that is not the title placeholder
        If (aShape.Type = msoPlaceholder) And Not (ShapesAreEqual(aShape, aSlide.shapes.Title)) Then
            Set SlideBodyShape = aShape
            Exit Function
        End If
    Next aShape
    
    'TODO: if this point is reached there is no slide body: this is a problem

End Function

Public Function ShapesAreEqual(Shape1 As Shape, shape2 As Shape) As Boolean
    If (Shape1.id = shape2.id) Then
        ShapesAreEqual = True
    Else
        ShapesAreEqual = False
    End If
End Function

Public Function ShapesAreCongruent(Shape1 As Shape, shape2 As Shape) As Boolean
    If (Shape1.Type = shape2.Type) _
                And (Shape1.top = shape2.top) _
                And (Shape1.left = shape2.left) _
                And (Shape1.width = shape2.width) _
                And (Shape1.height = shape2.height) Then
        ShapesAreCongruent = True
    Else
        ShapesAreCongruent = False
    End If
End Function

Private Function TitleSlideTextRange() As TextRange
    Set TitleSlideTextRange = TitleSlide.shapes.Title.TextFrame.TextRange
End Function


' **********************************************************************
' **********************************************************************
' Viewing
' **********************************************************************
' **********************************************************************

Public Sub RefreshSlide(aSlide As Slide, studentGdeMode As Boolean)
    On Error Resume Next
    
    ' reapply the layout master
    ' aSlide.Layout = aSlide.Layout
    InstructorNoteOn(aSlide).Visible = Not studentGdeMode
End Sub

Private Sub RefreshView(developerMode As Boolean, studentGdeMode As Boolean)
    On Error Resume Next

    Dim aSlide As Slide
    
    StudentGuideMode = studentGdeMode
    
    SetPrintOptions
    CreateOrDeleteFinalBlankPage (developerMode)
    'UpdateSummarySlide
    'UpdateGlobalTextValues
    ShowOrHideDevelopmentCues (developerMode)
    'SetSlideShowTransitionAndEffect
    ViewNotesPage
    
    PositionMasterElements
    
    For Each aSlide In ActivePresentation.Slides
        Call RefreshSlide(aSlide, studentGdeMode)
    Next aSlide
    
End Sub

Sub ViewDeveloperGuide()
'
' Reposition and redimension objects on the page.
'
    RefreshView True, False
End Sub

Sub ViewInstructorGuide()
'
' Display the presentation in the instructor guide format.
'
    RefreshView False, False
End Sub

Sub ViewStudentGuide()
'
' Display the presentation in the student guide format.
'
    RefreshView False, True
End Sub

Private Sub ViewNotesPage()
    ActiveWindow.ViewType = ppViewNotesPage
    ActiveWindow.View.ZoomToFit = True
End Sub

Private Sub ViewSlide()
    ActiveWindow.ViewType = ppViewSlide
End Sub

Private Sub ViewSlideSorter()
    ActiveWindow.ViewType = ppViewSlideSorter
End Sub


' **********************************************************************
' **********************************************************************
' This
' **********************************************************************
' **********************************************************************

Public Function ThisNotesPage() As Slide
'
' The notespage is the last one that does not error
'
    On Error Resume Next
    Set ThisNotesPage = ThisSlide
    Set ThisNotesPage = ThisNotesPage.Parent
    Set ThisNotesPage = ThisNotesPage.Parent
End Function

Public Function ThisSlide() As Slide
'
' Answer the active slide.
'
    Set ThisSlide = ActiveWindow.Selection.SlideRange(1)
End Function

Public Function ThisNotesMaster() As Master
'
' Answer the master for the active presentation.
'
    Set ThisNotesMaster = ActivePresentation.NotesMaster
End Function

Public Function ThisSlideMaster() As Master
'
' Answer the master for the active presentation.
'
    Set ThisSlideMaster = ActivePresentation.SlideMaster
End Function

Public Function ThisTitleMaster() As Master
'
' Answer the master for the active presentation.
'
    Set ThisTitleMaster = ActivePresentation.TitleMaster
End Function

