unit SharedPointer;
{$mode delphi}

interface

type
  // TDeallocator = {$IFNDEF FPC}reference to{$ENDIF} procedure(AObj: TObject);

TShared<T: class> = record
private
  FFreeTheValue: IInterface;
public
  constructor Create(AValue: T);
  procedure Assign(AValue: T);
  procedure SetDeallocator(ADealloc: TDeallocator);
  function Temporary: T;
  function Cast<TT: class>: TShared<TT>;
  function Release: T;
end;

TFreeTheValue = class(TInterfacedObject)
public
  FObjectToFree: TObject;
  FCustomDeallocator: TDeallocator;
  constructor Create(AObjToFree: TObject);
  destructor Destroy; override;
end;

TSharedList<T: class> = record
private
  FList: array of TShared<T>;
  function GetItem(I: Integer): T;
  function GetSharedItem(I: Integer): TShared<T>;
public
  function Count: Integer;
  property Items[I: Integer]: T read GetItem;
  property SharedItems[I: Integer]: TShared<T> read GetSharedItem; default;
  procedure Add(AObject: TShared<T>);
  procedure Clear;
end;

implementation

function TShared<T>.Temporary: T;
begin
  Result := nil;
  if (( FFreeTheValue <> nil) and ((FFreeTheValue as TFreeTheValue).FObjectToFree <> nil)) then
    Result := ((FFreeTheValue as TFreeTheValue).FObjectToFree as T);
end;

constructor TFreeTheValue.Create(AObjToFree: TObject);
begin
  FObjectToFree := AObjToFree;
end;

destructor TFreeTheValue.Destroy;
begin
  if Assigned(FObjectToFree) then
  begin
    if Assigned(FCustomDeallocator) then
    begin
      FCustomDeallocator(FObjectToFree);
      FObjectToFree := nil;
      FCustomDeallocator := nil;
    end
    else
    begin
      FObjectToFree.Free;
      FObjectToFree := nil;
    end;
  end;
end;

function TShared<T>.Cast<TT>: TShared<TT>;
begin
  Result := TShared<TT>.Create(nil);
  Result.FFreeTheValue := FFreeTheValue;
end;

constructor TShared<T>.Create(AValue: T);
begin
  Assign(AValue);
end;

procedure TShared<T>.Assign(AValue: T);
begin
  FFreeTheValue := TFreeTheValue.Create(AValue);
end;

function TShared<T>.Release: T;
begin
  Result := Temporary;
  (FFreeTheValue as TFreeTheValue).FObjectToFree := nil;
end;

procedure TShared<T>.SetDeallocator(ADealloc: TDeallocator);
begin
  (FFreeTheValue as TFreeTheValue).FCustomDeallocator := ADealloc;
end;

procedure TSharedList<T>.Add(AObject: TShared<T>);
begin
  SetLength(FList,( Length(FList)+1));
  FList[(Length(FList)-1)] := AObject;
end;

procedure TSharedList<T>.Clear;
begin
  SetLength(FList, 0);
end;

function TSharedList<T>.Count: Integer;
begin
  Result := Length(FList);
end;

function TSharedList<T>.GetSharedItem(I: Integer): TShared<T>;
begin
  Result := FList[I];
end;

function TSharedList<T>.GetItem(I: Integer): T;
begin
  Result := GetSharedItem(I).Temporary;
end;

end.
