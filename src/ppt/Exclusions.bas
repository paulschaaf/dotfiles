Attribute VB_Name = "Exclusions"
' **********************************************************************
' **********************************************************************
' Course Exclusions
' **********************************************************************
' **********************************************************************

Public Const CourseExclusionsName = "CourseExclusions"
Public Const CourseExclusionsLabelName = "CourseExclusionsLabel"


Sub AddExclusionNote()
'
' Adds an exclusion note box to the current slide (if it doesn't already have one). When a course
' is prepared all slides that are excluded from that course are removed.
'
    ActiveWindow.ViewType = ppViewSlide
    AddExclusionNoteTo ThisSlide
End Sub

Public Function AllCourseExclusions() As String
    Dim Exclusions As String
    Dim newStr As String
    Dim aSlide As Slide
    
    For Each aSlide In ActivePresentation.Slides
        newStr = CourseExclusionsOn(aSlide)
        If Len(newStr) > 0 Then
            If Len(Exclusions) > 0 Then
                Exclusions = Exclusions + ", "
            End If
            Exclusions = Exclusions + Str(aSlide.slideNumber) + ": " + newStr
        End If
    Next aSlide
    AllCourseExclusions = Exclusions
End Function

Public Function CourseExclusionsOn(aSlide As Slide) As String
    On Error GoTo ErrorHandler
    CourseExclusionsOn = ElementOnSlideWithIdentifier(aSlide, CourseExclusionsName).TextFrame.TextRange.text
    Exit Function
    
ErrorHandler:
    CourseExclusionsOn = ""
End Function

Public Function SlideExcludes(aSlide As Slide, aCourse As String) As Boolean
    Dim match As Boolean
    Dim Exclusions As String
    
    Exclusions = CourseExclusionsOn(aSlide)
    If (LCase(Exclusions) = "all") Then
        SlideExcludes = True
    Else
        match = InStr(1, Exclusions, aCourse, 1)
        SlideExcludes = Not (match = 0)
    End If
End Function

Public Function MasterCourseExclusions() As Shape
    Set MasterCourseExclusions = ThisSlideMaster.shapes(CourseExclusionsName)
End Function

Public Function MasterCourseExclusionsLabel() As Shape
    Set MasterCourseExclusionsLabel = ThisSlideMaster.shapes(CourseExclusionsLabelName)
End Function

Public Function SlidesExcludingCourse(aCourse As String) As Collection
    Dim aSlide As Slide
    Dim Exclusions As New Collection
    
    For Each aSlide In ActivePresentation.Slides
        If SlideExcludes(aSlide, aCourse) Then
            Exclusions.Add aSlide
        End If
    Next aSlide
    Set SlidesExcludingCourse = Exclusions
    
End Function

Sub AddExclusionNoteTo(aSlide As Slide)
'
' Adds a section for exclusion notes, unless one already exists.
'
    On Error GoTo ErrorHandler
    
    Dim aNewShape As Shape
    
    MasterCourseExclusions.Copy
    Set aNewShape = aSlide.shapes.Paste(1)
    With aNewShape
        .name = CourseExclusionsName
        .Visible = True
        .TextFrame.TextRange.Select
    End With
        
    Exit Sub

ErrorHandler:
    Select Case Err.Number
        Case 70  'Shape already exist, so delete the new one
            aNewShape.Delete
            Err.Clear
            Exit Sub
        Case Else
            On Error GoTo 0
    End Select
    
    Resume

End Sub


