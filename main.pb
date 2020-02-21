; iEmbed v1.0
; A cli tool, for embed local images in html & markdown files
; Copyright (c) 2020, Petros Kyladitis <petros.kyladitis@gmail.com>
;
; This is free software distributed under the FreeBSD license, for details see at 'license.txt'

#F_ENCODE = 0
#F_PARSE  = 1
#F_OUTPUT = 2
#SQUOTE$  = "'"

;Encode given file using Base64 method and return it as string
Procedure.s EncodeFile(file$)
  ;check if file exist
  If GetFileAttributes(file$) & #FILE_ATTRIBUTE_DIRECTORY = #False
    If ReadFile(#F_ENCODE, file$, #PB_File_SharedRead)
      len.q = Lof(#F_ENCODE)                           
      *buf = AllocateMemory(len)       
      If *buf
        ReadData(#F_ENCODE, *buf, len) 
      EndIf
    EndIf
    CloseFile(#F_ENCODE) 
    ProcedureReturn Base64Encoder(*buf, len)
  Else
    ProcedureReturn #Null$
  EndIf
EndProcedure

;Generate data uri scheme for an supported image file, based on its extension
Procedure.s DataUriScheme(file$)
  ext$ = LCase(GetExtensionPart(file$))
  scheme$ = "data:image/"
  Select ext$
    Case "png"
      scheme$ = scheme$ + "png;base64,"
    Case "jpg"
      scheme$ = scheme$ + "jpeg;base64,"
    Case "jpeg"
      scheme$ = scheme$ + "jpeg;base64,"
    Case "gif"
      scheme$ = scheme$ + "gif;base64,"
    Case "svg"
      scheme$ = scheme$ + "svg+xml;base64,"
    Default
      scheme$ = #Null$
  EndSelect
  ProcedureReturn scheme$
EndProcedure

;Parse MD file and return it as string with images embedded as base64 data uri schemes
Procedure.s ParseMD(file$)
  newtext$ = #Null$
  If ReadFile(#F_PARSE, file$)
    NewList imgRefs.s()
    text$ = ReadString(#F_PARSE, #PB_File_IgnoreEOL)
    imgStart.q = -2
    imgEnd.q = 0
    refEnd.q = 0
    Repeat
      imgUrl$ = #Null$
      
      imgStart = FindString(text$, "![", imgStart + 2)
      If imgStart
        ;try to parse inline style image
        imgEnd.q = FindString(text$, ")", imgStart + 2)
        imgRefStart.q = FindString(text$, "][", imgStart + 2)
        If imgEnd < imgRefStart Or imgRefStart = 0
          imgInlineUrlStart.q = FindString(text$, "](", imgStart)
          imgInlineUrlEnd.q = 0
          If imgInlineUrlStart And imgInlineUrlStart < imgEnd
            imgInlineUrlStart = imgInlineUrlStart + 2
            imgInlineUrlEnd = FindString(text$, #DQUOTE$, imgInlineUrlStart)
            ;if not found double quotes (image title), search for alternative script with single quotes
            If Not imgInlineUrlEnd Or imgInlineUrlEnd > imgEnd
              imgInlineUrlEnd = FindString(text$, #SQUOTE$, imgInlineUrlStart)
              ;if not found image title, search for image end mark
              If Not imgInlineUrlEnd Or imgInlineUrlEnd > imgEnd
                imgInlineUrlEnd = imgEnd
              EndIf
            EndIf
            If imgInlineUrlEnd
              imgUrl$ = Trim(Mid(text$, imgInlineUrlStart, imgInlineUrlEnd - imgInlineUrlStart))
              encoded$ = EncodeFile(imgUrl$)
              If encoded$ <> #Null$
                encoded$ = DataUriScheme(imgUrl$) + encoded$
                text$ = ReplaceString(text$, imgUrl$, encoded$, #PB_String_NoCase, imgStart, 1)
                imgStart = imgStart + Len(encoded$)
              EndIf
            EndIf
          EndIf
        Else 
          ;try to parse reference style image 
          imgRefStart = imgRefStart + 2
          imgRefEnd.q = FindString(text$, "]", imgRefStart)
          If imgRefEnd
            imgRef$ = Mid(text$, imgRefStart, imgRefEnd - imgRefStart)
            imgRefExist.b = #False
            ForEach imgRefs()
              If imgRefs() = imgRef$
                imgRefExist = #True
              EndIf 
            Next
            If Not imgRefExist
              AddElement(imgRefs())
              imgRefs() = imgRef$
            EndIf
          EndIf
        EndIf
      EndIf
    Until Not imgStart
    
    ;parse image references
    ForEach imgRefs()
      ref$ = "[" + imgRefs() + "]:"
      refStart.q = FindString(text$, ref$)
      refEnd.q = refStart + Len(ref$)
      lineEnd.q = FindString(text$, #LF$, refEnd)
      imgTitleStart = FindString(text$, #DQUOTE$, refEnd)
      ;if not found double quotes (image title), search for alternative script with single quotes
      If Not imgTitleStart Or  imgTitleStart > lineEnd
        imgTitleStart = FindString(text$, #SQUOTE$, refEnd)
      EndIf
      ;if image title found, read url until there, else read until the end of the line
      If imgTitleStart
        imgUrl$ = Trim(Mid(text$, refEnd, imgTitleStart - refEnd))
      Else
        imgUrl$ = Trim(Trim(Mid(text$, refEnd, lineEnd - refEnd),#CR$))
      EndIf
      encoded$ = EncodeFile(imgUrl$)
      If encoded$ <> #Null$
        encoded$ = DataUriScheme(imgUrl$) + encoded$
        text$ = ReplaceString(text$, imgUrl$, encoded$, #PB_String_NoCase, refEnd, 1)
      EndIf
    Next
    CloseFile(#F_PARSE)
  EndIf
  ProcedureReturn text$
EndProcedure

;Parse HTML file and return it as string with images embedded as base64 data uri schemes
Procedure.s ParseHTML(file$)
  newtext$ = #Null$
  If ReadFile(#F_PARSE, file$)
    text$ = ReadString(#F_PARSE, #PB_File_IgnoreEOL)
    imgStart.q = -5
    imgEnd.q = 0
    srcStart.q = 0
    srcEnd.q = 0
    Repeat
      imgUrl$ = #Null$
      
      imgStart = FindString(text$, "<img", imgStart + 4, #PB_String_NoCase)
      If imgStart
        imgEnd.q = FindString(text$, ">", imgStart + 4)
        If imgStart < imgEnd ;search for src property with " enclosing mark
          srcStart = FindString(text$, "src=" + #DQUOTE$, imgStart + 5, #PB_String_NoCase)
          If Not srcStart ;search for src property with ' enclosing mark
            srcStart = FindString(text$, "src='", imgStart + 5, #PB_String_NoCase)
            If Not srcStart ;search for src property without enclosing marks
              srcStart = FindString(text$, "src=", imgStart + 5, #PB_String_NoCase)
              srcEnd = FindString(text$, " ", srcStart + 5)
              If Not srcEnd
                srcEnd = FindString(text$, ">", srcStart + 5)
              EndIf
              srcStart = srcStart - 1
            Else
              srcEnd = FindString(text$, #SQUOTE$, srcStart + 5)
            EndIf
          Else
            srcEnd = FindString(text$, #DQUOTE$, srcStart + 5)
          EndIf
          If srcStart And srcEnd And (srcStart < srcEnd)
            imgUrl$ = Mid(text$, srcStart + 5, srcEnd - srcStart - 5)
            encoded$ = EncodeFile(imgUrl$)
            If encoded$ <> #Null$
              encoded$ = DataUriScheme(imgUrl$) + encoded$
              text$ = ReplaceString(text$, imgUrl$, encoded$, #PB_String_NoCase, srcStart + 5, 1)
              imgStart = imgStart + Len(encoded$) - Len(imgUrl$)
            Else
              imgStart = imgEnd
            EndIf
          Else
            imgStart = imgEnd
          EndIf
        EndIf
      EndIf
    Until Not imgStart
    CloseFile(#F_PARSE)
    ProcedureReturn text$
  EndIf
EndProcedure

;coordinates the embedding process, with the input files
Procedure.s Embed(inputFile$, outputFile$)
  ret$ = #Empty$ ;returned message
  out$ = #Empty$ ;output source
  
  ;check if file exist
  If GetFileAttributes (inputFile$) & #FILE_ATTRIBUTE_DIRECTORY = #False
    
    ;if output file not given, generate its name by added the '-embed' before input's extension
    If Not Len(outputFile$)
      outputFile$ = GetPathPart(inputFile$) + GetFilePart(inputFile$, #PB_FileSystem_NoExtension) + "-embed." + GetExtensionPart(inputFile$)
    EndIf
    
    ;check if output file can be created
    If Not CreateFile(#F_OUTPUT, outputFile$, #PB_File_SharedWrite)
      ret$ = "Bad output filename, or can't create file there"
    EndIf
    
    ;if everything is ok (no returned message) continue
    If Not Len(ret$)
      inputExt$ = UCase(GetExtensionPart(inputFile$))
      
      ;check if input file is html or md by file's extension
      Select inputExt$
        Case "MD"
          out$ = ParseMD(inputFile$)
        Case "HTML", "HTM"
          out$ = ParseHTML(inputFile$)
        Default
          ret$ = "Only .html and .md files are supported"
      EndSelect
      
      ;if input file is acceptable (no returned message) continue to embedding process
      If Not Len(ret$)
        If OpenFile(#F_OUTPUT, outputFile$, #PB_File_SharedWrite)
          WriteString(#F_OUTPUT, out$)
        Else
          ret$ = "Error writing output file"
        EndIf
      EndIf
      
    EndIf
  Else
    ret$ = "Input file not found!"
  EndIf
  
  ;if everything is ok (no returned message) set succeed message
  If Not Len(ret$)
    ret$ = "Done! " + #CRLF$ + GetFilePart(outputFile$) + " created."
  EndIf
  
  ProcedureReturn ret$
EndProcedure

;Shows program's help message
Procedure ShowHelp()
  PrintN("iEmbed v1.0 - Embed local images in html & markdown files")
  PrintN("Copyright (c) 2020, Petros Kyladitis")
  PrintN(#CRLF$ + "Usage: iembed input_file [output_file]")
EndProcedure

;Program's entry point
If(OpenConsole())  
  paramsNum.i = CountProgramParameters()
  param.s = ProgramParameter()
  If paramsNum > 0 And paramsNum < 3
    If param = "/?" Or param = "-?" Or param = "-h"
      ShowHelp()
    Else
      Print(Embed(param, ProgramParameter()))
    EndIf
  Else
    ShowHelp()
  EndIf  
  CloseConsole()
EndIf
; IDE Options = PureBasic 5.70 LTS (Windows - x86)
; ExecutableFormat = Console
; CursorPosition = 261
; FirstLine = 226
; Folding = --
; EnableXP
; Executable = iembed.exe