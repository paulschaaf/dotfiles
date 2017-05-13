Attribute VB_Name = "SlideCleanup"
' **********************************************************************
' **********************************************************************
' Slide Cleanup
' **********************************************************************
' **********************************************************************

' **********************************************************************
' Slide titles
Public Const ModuleObjectivesSlideTitle = "Module Objectives"
Public Const ModuleSummarySlideTitle = "Module Summary"

Public Const WarnOnMissingObjectivesSlide = False
Public Const WarnOnMissingSummarySlide = False


Public Sub BringCalloutsToFront()
' Brings all callout boxes on the current slide to the top layer.
'
    Dim aShape As Shape
    Dim aShapes As New Collection
    
    For Each aShape In ThisSlide.shapes
        If aShape.AutoShapeType = msoShapeRectangularCallout Then
            aShapes.Add aShape
        End If
    Next aShape
    
    BringToFrontObjectsIn aShapes
End Sub

Public Sub BringConnectorsAndArrowsToFront()
' Brings all connecters and arrows on the current slide to the top layer.
'
    Dim aShape As Shape
    Dim aShapes As New Collection
    
    For Each aShape In ThisSlide.shapes
        If (aShape.Connector = True) Or (aShape.Type = msoLine) Then
            aShapes.Add aShape
        End If
    Next aShape
    
    BringToFrontObjectsIn aShapes
End Sub

Public Sub BringToFrontObjectsIn(aCollection As Collection)
    Do While aCollection.count > 0
        aCollection.Item(1).ZOrder msoBringToFront
        aCollection.Remove (1)
    Loop
End Sub

Private Function IndexOfObjectivesSlide() As Long
    IndexOfObjectivesSlide = IndexOfSlideWithTitle(ModuleObjectivesSlideTitle)
End Function

Private Function IndexOfSummarySlide() As Long
    IndexOfSummarySlide = IndexOfSlideWithTitle(ModuleSummarySlideTitle)
End Function

Private Function IndexOfSlideWithTitle(aTitle As String) As Long

    Dim aSlide As slide
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

Sub SetSlideShowTransitionAndEffect()

    Dim button As CommandBarButton
    
    On Error Resume Next
    
    ViewSlideSorter
    ActivePresentation.Slides.Range.SlideShowTransition.EntryEffect = ppEffectNone

    ActivePresentation.Slides.Range.Select
    Set button = CommandBars.ActiveMenuBar.Controls("Sli&de Show").Controls("&Preset Animation").Controls("&Off") '8, 1
    button.Execute
    
    'For some reason directly calling ViewNotesPage will undo the above work
    CommandBars.ActiveMenuBar.Controls("&View").Controls("&Notes Page").Execute
End Sub

Public Function ShapesAreEqual(Shape1 As Shape, shape2 As Shape) As Boolean
    If (Shape1.id = shape2.id) Then
        ShapesAreEqual = True
    Else
        ShapesAreEqual = False
    End If
End Function

Function SlideBodyShape(aSlide As slide) As Shape

    Dim aShape As Shape
    
    On Error Resume Next
    
    For Each aShape In aSlide.shapes
        ' find the first placeholder that is not the title placeholder
        If (aShape.Type = msoPlaceholder) And Not (ShapesAreEqual(aShape, aSlide.shapes.Title)) Then
            Set SlideBodyShape = aShape
            Exit Function
        End If
    Next aShape
    
    'TODO: if this point is reached there is no slide body; this is a problem
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

Private Function TitleSlide() As slide
    Dim aSlide As slide
    
    For Each aSlide In ActivePresentation.Slides
        If aSlide.Layout = ppLayoutTitle Then
            Set TitleSlide = aSlide
            Exit Function
        End If
    Next aSlide
End Function

Private Function TitleSlideTextRange() As TextRange
    Set TitleSlideTextRange = TitleSlide.shapes.Title.TextFrame.TextRange
End Function

Sub UpdateGlobalTextValues()
    On Error Resume Next
    UpdateSectionName
    
    With ThisNotesMaster
        .shapes("ModuleName").Visible = True
        .shapes("ModuleNumber").Visible = True
        .shapes(ModuleNameBoxName).Visible = True
        .shapes(PageNumberBox).Visible = True
    End With
    
    If isSection() And Not isTemplate() Then
        ThisSlideMaster.shapes(ModuleNameBoxName).TextFrame.TextRange.text = ModuleName
        TitleSlideTextRange.text = ModuleName
    Else
        With ThisNotesMaster
            'set module name
            .shapes("ModuleName").TextFrame.TextRange.text = ModuleName

            'set module number
            With .shapes("ModuleNumber").TextFrame.TextRange
                .text = SectionName + ", " + ModuleNumberString + "."
                .InsertAfter.InsertSlideNumber
            End With
        End With
        UpdatePageCount
        
        ThisSlideMaster.shapes(ModuleNameBoxName).TextFrame.TextRange.text = ModuleNumberFormalString + ": " + ModuleName
        TitleSlideTextRange.text = ModuleNameFormalString
    End If

End Sub

Sub UpdateSummarySlide()
    Dim objectivesIndex, summaryIndex
    
    objectivesIndex = IndexOfObjectivesSlide
    summaryIndex = IndexOfSummarySlide
    
    If objectivesIndex = 0 Then
        If WarnOnMissingObjectivesSlide Then
            Call MsgBox("There is no '" + ModuleObjectivesSlideTitle + "' slide!", vbOKOnly, "Error: Cannot update Summary slide")
        End If
    ElseIf summaryIndex = 0 Then
        If WarnOnMissingSummarySlide Then
            Call MsgBox("'" + ModuleSummarySlideTitle + "' slide not found!", vbOKOnly, "Error: Cannot update Summary slide")
        End If
    Else
        Dim summary As slide
        Dim aTextRange As TextRange
        Dim summarySlide As slide
        Dim discardText
        
        'copy the body text of the objectives slide
        SlideBodyShape(ActivePresentation.Slides(objectivesIndex)).Copy
        Set summarySlide = ActivePresentation.Slides(summaryIndex)
        SlideBodyShape(summarySlide).Delete
        summarySlide.shapes.Paste
        
        Set aTextRange = SlideBodyShape(summarySlide).TextFrame.TextRange
        
        'update it to summary language
        Call aTextRange.Replace("After completing this module you will be able to", "This module showed you how to")
        
        'remove the justification section
        'discardText = Split(aTextRange.Text, "Why you")(1)
        'aTextRange.Characters(discardTextIdx, Len(aTextRange.Text)).Cut
        'Call aTextRange.Replace("Why you" + discardText, "")
        
        'TODO perhaps get rid of the trailing whitespace here
    End If
End Sub

Private Sub UpdateSectionName()
    Dim aString
    
    If (developerMode) Then
        If isTemplate Then
            aString = "Template"
        Else
            aString = "Developer Guide"
        End If
    ElseIf (studentGuideMode) Then
        aString = "Student Guide"
    Else
        aString = "Instructor Guide"
    End If
    
    UpdateSectionNameWith (aString)
End Sub

Private Sub UpdateSectionNameWith(aLabel As String)
    On Error Resume Next
    
    Dim aShape As Shape
    Dim aString
    
    Set aShape = SectionNameBox
    aString = aLabel + " version " + SectionPublicVersionNumber
    aShape.TextFrame.TextRange.text = aString
End Sub

Private Sub ViewSlide()
    ActiveWindow.ViewType = ppViewSlide
End Sub

Private Sub ViewSlideSorter()
    ActiveWindow.ViewType = ppViewSlideSorter
End Sub

