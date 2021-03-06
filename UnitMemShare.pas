unit UnitMemShare;

interface
uses System.Generics.Collections;

const MEM_SIZE_MIN = 4;

type
  TSharedMemoryBuffer = Class
  protected
    constructor Create(const Name: string; SizeKbB: Integer = MEM_SIZE_MIN); //RAII
    function Share(accessMemoryMode: Cardinal = 0): Pointer;
    destructor Destroy(); override;
  private
    var
      _HMapping: THandle;
      _Name: String;
      _AccessMode: Cardinal;
      _MemoryOffset: NativeUInt;
      _MemorySizeBytes: NativeUInt;
  private
    function getAccessMode(): Cardinal;
  public
    property Name: String read _Name;
    property AccessMode: Cardinal read getAccessMode write _AccessMode;
    property MemoryOffset: NativeUInt read _MemoryOffset write _MemoryOffset;
    property MemorySizeBytes: NativeUInt read _MemorySizeBytes write _MemorySizeBytes;
  public
    class function OpenShare(Name: String): Pointer;
    class function CloseShare(Name: String): Pointer;
    class function New(Name: string): TSharedMemoryBuffer;
    class function ShareMemory(Name: string = ''): Pointer;
    class function Peak(): String;
    class function PeakShare(var Name: String): TSharedMemoryBuffer;
    class function Free(Name: String = ''): Boolean;
  private
    class var SharesPool: TDictionary<String, TSharedMemoryBuffer>;
    class var _DefaultAccessMode: Cardinal;
    class function getDefaultAccessMode: Cardinal; static;
    class procedure setDefaultAccessMode(accessMode: Cardinal); static;
  public
    class property DefaultMemoryAccessMode: Cardinal read getDefaultAccessMode write setDefaultAccessMode;
  End;

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
{ DONE :
Implement as class RAII
with ability to change share options and close handle at destructor }

function CreateMapping(MeMpName: String =''; SizeKbB: Integer = MEM_SIZE_MIN): THandle;
var RequestedMemSize: Int64;
var SizeLow, SizeHigh: DWORD;
begin
  RequestedMemSize := SizeKbB * BYTES_KbB;
  SizeLow := RequestedMemSize AND $00000000FFFFFFFF;
  SizeHigh := RequestedMemSize AND $FFFFFFFF00000000;

  if MeMpName = '' then
    MeMpName := GenerateShareName();

  RESULT := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, SizeHigh, SizeLow, PChar(MeMpName));
end;

function OpenMapping(MeMpName: String; AccessMode: Cardinal): THandle;
begin
  RESULT :=OpenFileMapping(AccessMode, True, PChar(MeMpName));
end;

function ObtainViewAccess(HMapping: THandle; AccessMode: Cardinal; Offset: NativeUInt = 0; Size: NativeUInt = 0): Pointer;
begin
{$IFDEF MSWINDOWS}
  var OffsetLow: DWORD := Offset AND $00000000FFFFFFFF;
  var OffsetHigh: DWORD := (Offset AND $FFFFFFFF00000000) SHR $20;
  //{default value --} AccessMode: DWORD = FILE_MAP_ALL_ACCESS 
  RESULT := MapViewOfFile(HMapping, DWORD(AccessMode), OffsetHigh, OffsetLow, Size);
{$ELSE}
    { TODO : implement for POSIX platforms }
    raise ENotImplemented.Create('Memory Mapping is Not Implemented for the current platform');
{$ENDIF}
end;

function CreateMemoryShare(MeMpName: String =''; SizeKbB: Integer = MEM_SIZE_MIN): Pointer;
begin
  var HMapping := CreateMapping(MeMpName, SizeKbB);
  {got handle}
  RESULT := ObtainViewAccess(HMapping, FILE_MAP_ALL_ACCESS);
end;

procedure MemCopy(pSource, pDestination: Pointer; Size: NativeInt);
begin
{$IFDEF MSWINDOWS}
  CopyMemory(pDestination, pSource, Size);
{$ELSE}
  System.Move(pSource, pDestination, Size);
{$ENDIF}
end;
{ TSharedMemoryBuffer }

class function TSharedMemoryBuffer.CloseShare(Name: String): Pointer;
begin
  TSharedMemoryBuffer.Free(Name);
end;

constructor TSharedMemoryBuffer.Create(const Name: string; SizeKbB: Integer);
begin
  _Name := Name;
  _AccessMode := DefaultMemoryAccessMode;

  if SizeKbB > 0 then // create new share
    _HMapping := CreateMapping(Name, SizeKbB)
  else               // try open existing share
    _HMapping := OpenMapping(Name, AccessMode);
end;

destructor TSharedMemoryBuffer.Destroy;
begin
  CloseHandle(_HMapping);
  inherited;
end;

class function TSharedMemoryBuffer.Free(Name: String): Boolean;
var Share: TSharedMemoryBuffer;
begin
  if Name = '' then
    Name := Peak();
  if Name = '' then
    EXIT(True);

  if SharesPool.TryGetValue(Name, Share) then
  begin
    FreeAndNil(Share);
    SharesPool.Remove(Name);
  end;

  RESULT := SharesPool.Count = 0;
end;

function TSharedMemoryBuffer.getAccessMode: Cardinal;
begin
  if _AccessMode = 0 then
    _AccessMode := DefaultMemoryAccessMode;
  RESULT := _AccessMode;
end;

class function TSharedMemoryBuffer.getDefaultAccessMode: Cardinal;
begin
  if _DefaultAccessMode = 0 then
    _DefaultAccessMode := FILE_MAP_ALL_ACCESS;
  RESULT := _DefaultAccessMode;
end;

class function TSharedMemoryBuffer.New(Name: string): TSharedMemoryBuffer;
begin
  if SharesPool.ContainsKey(Name) then
    EXIT(nil);

  var newShare := TSharedMemoryBuffer.Create(Name);
  SharesPool.Add(Name, newShare);
  RESULT := newShare;
end;

class function TSharedMemoryBuffer.OpenShare(Name: String): Pointer;
var Share: TSharedMemoryBuffer;
begin
  if Name = '' then
    EXIT(nil);

  Share := TSharedMemoryBuffer.Create(Name, 0);
  SharesPool.AddOrSetValue(Name, Share);
  RESULT := Share.Share();
end;

class function TSharedMemoryBuffer.Peak(): String;
  begin
    //DONE: implement
    var Nshares := SharesPool.Keys.Count;
    if Nshares > 0 then
      RESULT := SharesPool.Keys.ToArray[Nshares-1]
    else
      RESULT := '';
  end;

class function TSharedMemoryBuffer.PeakShare(var Name: String): TSharedMemoryBuffer;
begin
  if Name = '' then
    Name := Peak();
  SharesPool.TryGetValue(Name, RESULT);
end;

class procedure TSharedMemoryBuffer.setDefaultAccessMode(accessMode: Cardinal);
begin
  _DefaultAccessMode := accessMode;
end;

class function TSharedMemoryBuffer.ShareMemory(Name: string): Pointer;
var Share: TSharedMemoryBuffer;
begin
  if SharesPool.TryGetValue(Name, Share) then
    RESULT := Share.Share()
  else
    RESULT := TSharedMemoryBuffer.New(Name).Share();
end;

function TSharedMemoryBuffer.Share(accessMemoryMode: Cardinal): Pointer;
begin
  if accessMemoryMode = 0 then
    accessMemoryMode := AccessMode
  else
    AccessMode := accessMemoryMode;
  RESULT := ObtainViewAccess(_HMapping, _AccessMode, _MemoryOffset, _MemorySizeBytes);
end;

initialization
  TSharedMemoryBuffer.SharesPool := TDictionary<String, TSharedMemoryBuffer>.Create();
finalization
  repeat until TSharedMemoryBuffer.Free();
  TSharedMemoryBuffer.SharesPool.Free;
end.
