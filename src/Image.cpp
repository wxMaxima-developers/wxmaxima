#include "Image.h"
#include <wx/mstream.h>
#include <wx/wfstream.h>

wxMemoryBuffer Image::ReadCompressedImage(wxInputStream *data)
{
  wxMemoryBuffer retval;

  char *buf = new char[8192];

  while(data->CanRead())
    {
      data->Read(buf,8192);
      size_t siz;
      retval.AppendData(buf,siz=data->LastRead());
    }
  
  delete [] buf;
  return retval;
}

wxBitmap Image::GetUnscaledBitmap()
{
  wxMemoryInputStream istream(m_compressedImage.GetData(),m_compressedImage.GetDataLen());
  wxImage img(istream, wxBITMAP_TYPE_ANY);
  wxBitmap bmp;
  if(img.Ok())
    bmp = wxBitmap(img);
  return bmp;
}

Image::Image()
{
  m_viewportWidth  = 640;
  m_viewportHeight = 480;
  m_scale          = 1;  
  m_scaledBitmap.Create (1,1);
}

Image::Image(wxMemoryBuffer image,wxString type)
{
  m_viewportWidth  = 640;
  m_viewportHeight = 480;
  m_scale          = 1;  
  m_scaledBitmap.Create (1,1);
  m_compressedImage = image;
  m_extension = type;
  wxImage Image;
  if(m_compressedImage.GetDataLen()>0)
    {
      wxMemoryInputStream istream(m_compressedImage.GetData(),m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
    } 
}

Image::Image(const wxBitmap &bitmap)
{
  m_viewportWidth  = 640;
  m_viewportHeight = 480;
  m_scale          = 1;  
  LoadImage(bitmap);
}

// constructor which loads an image
Image::Image(wxString image,bool remove, wxFileSystem *filesystem)
{
  m_viewportWidth  = 640;
  m_viewportHeight = 480;
  m_scale          = 1;
  m_scaledBitmap.Create (1,1);
  LoadImage(image,remove,filesystem);
}

wxSize Image::ToImageFile(wxString filename)
{
  wxFileName fn(filename);
  wxString ext = fn.GetExt();
  if(filename.Lower().EndsWith(GetExtension().Lower()))
  {
    wxFile file(filename,wxFile::write);
    if(!file.IsOpened())
      return wxSize(-1,-1);
    
    file.Write(m_compressedImage.GetData(), m_compressedImage.GetDataLen());
    if(file.Close())
      return wxSize(m_originalWidth,m_originalHeight);
    else
      return wxSize(-1,-1);
  }
  else
  {
    wxBitmap bitmap = GetUnscaledBitmap();
    wxImage image=bitmap.ConvertToImage();
    wxBitmapType mimetype = wxBITMAP_TYPE_ANY;
    if((ext.Lower() == wxT("jpg")) || (ext.Lower() == wxT("jpeg")))
      mimetype = wxBITMAP_TYPE_JPEG;
    else if(ext.Lower() == wxT("png"))
      mimetype = wxBITMAP_TYPE_PNG;
    else if(ext.Lower() == wxT("pcx"))
      mimetype = wxBITMAP_TYPE_PCX;
    else if(ext.Lower() == wxT("pnm"))
      mimetype = wxBITMAP_TYPE_PNM;
    else if((ext.Lower() == wxT("tif")) || (ext.Lower() == wxT("tiff")))
      mimetype = wxBITMAP_TYPE_TIFF;
    else if(ext.Lower() == wxT("xpm"))
      mimetype = wxBITMAP_TYPE_XPM;
    else if(ext.Lower() == wxT("ico"))
      mimetype = wxBITMAP_TYPE_ICO;
    else if(ext.Lower() == wxT("cur"))
      mimetype = wxBITMAP_TYPE_CUR;
    else
      return(wxSize(-1,-1));
    
    if(!image.SaveFile(filename,mimetype))
      return wxSize(-1,-1);
    return image.GetSize();
  }
}

wxBitmap Image::GetBitmap()
{
  // std::cerr<<m_scaledBitmap.GetWidth()<<"\n";
  ViewportSize(m_viewportWidth,m_viewportHeight,m_scale);

  // Let's see if we have cached the scaled bitmap with the right size
  if(m_scaledBitmap.GetWidth() == m_width)
    return m_scaledBitmap;


  // Seems like we need to create a new scaled bitmap.
  if(m_scaledBitmap.GetWidth()!=m_width)
    {
      wxImage img;
      if(m_compressedImage.GetDataLen() > 0)
	{
	  wxMemoryInputStream istream(m_compressedImage.GetData(),m_compressedImage.GetDataLen());
	  
	  img = wxImage(istream, wxBITMAP_TYPE_ANY);
	}

      if(img.Ok())
	m_scaledBitmap = wxBitmap(img);
      else
	{
	  // Create a "image not loaded" bitmap.
	  m_scaledBitmap.Create(400, 250);
	  
	  wxString error(_("Error"));
	  
	  wxMemoryDC dc;
	  dc.SelectObject(m_scaledBitmap);
	  
	  int width = 0, height = 0;
	  dc.GetTextExtent(error, &width, &height);
	  
	  dc.DrawRectangle(0, 0, 400, 250);
	  dc.DrawLine(0, 0,   400, 250);
	  dc.DrawLine(0, 250, 400, 0);
	  dc.DrawText(error, 200 - width/2, 125 - height/2);
	  
	  dc.GetTextExtent(error, &width, &height);
	  dc.DrawText(error, 200 - width/2, 150 - height/2);
	}
    }
  
  // Make sure we stay within sane defaults
  if(m_width<1)m_width = 1;
  if(m_height<1)m_height = 1;

  // Create a scaled bitmap and return it.
  wxImage img=m_scaledBitmap.ConvertToImage();
  img.Rescale(m_width, m_height,wxIMAGE_QUALITY_BICUBIC);
  m_scaledBitmap = wxBitmap(img,24);
  return m_scaledBitmap;
}

void Image::LoadImage(const wxBitmap &bitmap)
{
  // Convert the bitmap to a png image we can use as m_compressedImage
  wxImage image = bitmap.ConvertToImage();
  wxMemoryOutputStream stream;
  image.SaveFile(stream,wxBITMAP_TYPE_PNG);
  m_compressedImage.AppendData(stream.GetOutputStreamBuffer()->GetBufferStart(),
			       stream.GetOutputStreamBuffer()->GetBufferSize());

  // Set the info about the image.
  m_extension = wxT("png");
  m_originalWidth  = image.GetWidth();
  m_originalHeight = image.GetHeight();
  m_scaledBitmap.Create (1,1);
}

void Image::LoadImage(wxString image, bool remove,wxFileSystem *filesystem)
{
  m_compressedImage.Clear();
  m_scaledBitmap.Create (1,1);

  if (filesystem) {
    wxFSFile *fsfile = filesystem->OpenFile(image);
    if (fsfile) { // open successful

      wxInputStream *istream = fsfile->GetStream();

      m_compressedImage = ReadCompressedImage(istream);
    }

    // Closing and deleting fsfile is important: If this line is missing
    // opening .wxmx files containing hundreds of images might lead to a
    // "too many open files" error.
    delete fsfile;
  }
  else {
    wxFile file(image);
    if(file.IsOpened())
      {
	wxFileInputStream strm(file);
	bool ok=strm.IsOk();
	if(ok)
	    m_compressedImage = ReadCompressedImage(&strm);
	
	file.Close();
	if(ok && remove)
	  wxRemoveFile (image);
      }
  }

  wxImage Image;
  if(m_compressedImage.GetDataLen()>0)
    {
      wxMemoryInputStream istream(m_compressedImage.GetData(),m_compressedImage.GetDataLen());
      Image.LoadFile(istream);
    }
  
  m_extension = wxFileName(image).GetExt();

  if(Image.Ok())
    {
      m_originalWidth  = Image.GetWidth();
      m_originalHeight = Image.GetHeight();
    }
  else
    {
      // Leave space for an image showing an error message
      m_originalWidth  = 400;
      m_originalHeight = 250;
    }
  ViewportSize(m_viewportWidth,m_viewportHeight,m_scale);

}

void Image::ViewportSize(size_t viewPortWidth,size_t viewPortHeight,double scale)
{
  int width  = m_originalWidth;
  int height = m_originalHeight;
  m_viewportWidth = viewPortWidth;
  m_viewportHeight= viewPortHeight;
  m_scale = scale;

  // Ensure a minimum scaling for images.
  if(scale < 1.0) scale = 1.0;
  
  if((width == 0) || (height == 0))
    {
      m_width = 400;
      m_height = 250;
      return;
    }

  if(viewPortHeight < 100)
    viewPortHeight = 100;
  if(viewPortWidth < 100)
    viewPortWidth = 100;  
  
  // Shrink to .9* the canvas size, if needed
  if(scale * width > .9 * viewPortWidth)
    scale = .9 * viewPortWidth / width;
  if(scale * height > .9 * viewPortHeight)
    scale = .9 * viewPortHeight / height;

  // Set the width of the scaled image
  m_height = (int) (scale * height);
  m_width  = (int) (scale * width);

  // Clear this cell's image cache if it doesn't contain an image of the size
  // we need right now.
  if(m_scaledBitmap.GetWidth() != m_width)
    ClearCache();
}
