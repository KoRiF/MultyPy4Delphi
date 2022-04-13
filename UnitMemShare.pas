unit UnitMemShare;

interface
const MEM_SIZE_MIN = 4;

function GenerateShareName(): String;
function CreateMemoryShare(MeMpName: String =''; SizeKbB: Integer = MEM_SIZE_MIN): Pointer;
procedure MemCopy(pSource, pDestination: Pointer; Size: NativeInt);

implementation

uses SysUtils, Windows;


const BYTES_KbB = 1024;

function GenerateShareName: String;
var Guid: TGUID;
begin
  CreateGUID(Guid);
  RESULT := GUIDToString(Guid);
end;
{ TODO :
Implement as class
with ability to change share options and close handle at destructor }
function CreateMemoryShare(MeMpName: String =''; SizeKbB: Integer = MEM_SIZE_MIN): Pointer;
var RequestedMemSize: Int64;
var SizeLow, SizeHigh: DWORD;
begin
  RequestedMemSize := SizeKbB * BYTES_KbB;
  SizeLow := RequestedMemSize AND $00000000FFFFFFFF;
  SizeHigh := RequestedMemSize AND $FFFFFFFF00000000;

  if MeMpName = '' then
    MeMpName := GenerateShareName();

  var HMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, SizeHigh, SizeLow, PChar(MeMpName));
  {got handle}
  RESULT := MapViewOfFile(HMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
end;

procedure MemCopy(pSource, pDestination: Pointer; Size: NativeInt);
begin
{$IFDEF MSWINDOWS}
  CopyMemory(pDestination, pSource, Size);
{$ELSE}
  System.Move(pSource, pDestination, Size);
{$ENDIF}
end;
end.
