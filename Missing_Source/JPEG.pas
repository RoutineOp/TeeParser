unit JPEG;

interface

uses
  Graphics;

type
  TJPEGPerformance=(jpBestQuality, jpSpeed);

  TJPEGDefaults=packed record
      GrayScale            : Boolean;
      ProgressiveEncoding  : Boolean;
      CompressionQuality   : Integer;
      Dpi                  : Integer;
      PixelFormat          : TPixelFormat;
      ProgressiveDisplay   : Boolean;
      Performance          : TJPEGPerformance;
      Scale                : Integer;
      Smoothing            : Boolean;
  end;

  TJPEGImage=class(TBitmap)
  public
      GrayScale            : Boolean;
      ProgressiveEncoding  : Boolean;
      CompressionQuality   : Integer;
      Dpi                  : Integer;
      PixelFormat          : TPixelFormat;
      ProgressiveDisplay   : Boolean;
      Performance          : TJPEGPerformance;
      Scale                : Integer;
      Smoothing            : Boolean;
  end;

const
   JPEGDefaults:TJPEGDefaults=(
      GrayScale           : False;
      ProgressiveEncoding : False;
      CompressionQuality  : 95;
      Dpi                 : 0;
      PixelFormat	  : pf24Bit;
      ProgressiveDisplay  : False;
      Performance         : jpBestQuality;
      Scale               : 1;
      Smoothing           : True;
                              );

implementation

end.
