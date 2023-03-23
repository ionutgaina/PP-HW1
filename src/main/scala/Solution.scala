import util.MyUtil.{Sir, opSplit}
import util.Pixel
import util.Util
import util.Util.{getNeighbors, toGrayScale}

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]
  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](List(1, 4, 7, 4, 1), List(4, 16, 26, 16, 4), List(7, 26, 41, 26, 7), List(4, 16, 26, 16, 4), List(1, 4, 7, 4, 1)).map(_.map(_ / 273))
  val Gx: GrayscaleImage = List(List(-1, 0, 1), List(-2, 0, 2), List(-1, 0, 1))
  val Gy: GrayscaleImage = List(List(1, 2, 1), List(0, 0, 0), List(-1, -2, -1))

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    // Remove the newlines from the list of chars
    val withoutNewLine = image.foldRight(Nil: List[Sir])(opSplit('\n'));


    //Get the row where is size of the image
    //Make the list of chars into a string
    val sizeRow = {
      withoutNewLine.slice(1, 2).map(list => list.fold("")(_ + "" + _)).map(e => e.toString)
    };

    // Get the length of the image
    val length = sizeRow.map(e => e.split(" ")).map(e => e(0).toInt).head;

    // Get the list of pixels (as strings)
    val stringList = withoutNewLine.drop(3).map(list => list.fold("")(_ + "" + _)).map(e => e.toString);

    // Get the list of pixels (as Pixel objects)
    val pixelList = stringList.map(e => e.split(" ")).map(e => Pixel(e(0).toInt, e(1).toInt, e(2).toInt));

    // Group the list of pixels into a matrix
    val pixelMatrix = pixelList.grouped(length).toList;
    pixelMatrix;
  }

  def toStringPPM(image: Image): List[Char] = {
    val length = image.head.length;
    val width = image.length;

    val header = "P3\n" + length + " " + width + "\n255\n";
    val body = image.map(list => list.map(pixel => pixel.red + " " + pixel.green + " " + pixel.blue + "\n").fold("")(_ + "" + _)).fold("")(_ + "" + _);
    val result = (header + body).toList;
    result
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    image1.zip(image2).map(e => e._1 ++ e._2)
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    def rotate90(image: Image): Image = {
      image.transpose.reverse
    }

    degrees % 360 match {
      case 90 => rotate90(image)
      case 180 => rotate90(rotate90(image))
      case 270 => rotate90(rotate90(rotate90(image)))
      case _ => image
    }
  }

  def edgeDetection(image: Image, threshold: Double): Image = {
    /*
    * Convert a image to a grayscale image
    * using the util function toGrayScale
     */ def toGrayScaleImage(image: Image): GrayscaleImage = {
      image.map(list => list.map(pixel => toGrayScale(pixel)))
    }

    val gaussianBlurKernel: GrayscaleImage = List[List[Double]](List(1, 4, 7, 4, 1), List(4, 16, 26, 16, 4), List(7, 26, 41, 26, 7), List(4, 16, 26, 16, 4), List(1, 4, 7, 4, 1)).map(_.map(_ / 273))

    // convert the image to grayscale
    val grayImage = toGrayScaleImage(image)

    // apply the gaussian blur
    val blurredImage = applyConvolution(grayImage, gaussianBlurKernel)

    val Gx: GrayscaleImage = List(List(-1, 0, 1), List(-2, 0, 2), List(-1, 0, 1))

    // apply Gx
    val Mx = applyConvolution(blurredImage, Gx)

    val Gy: GrayscaleImage = List(List(1, 2, 1), List(0, 0, 0), List(-1, -2, -1))

    // apply Gy
    val My = applyConvolution(blurredImage, Gy)

    // apply the threshold
    Mx.zip(My).map(e => e._1.zip(e._2).map(e => {
      if (Math.abs(e._1) + Math.abs(e._2) > threshold) {
        Pixel(255, 255, 255)
      } else {
        Pixel(0, 0, 0)
      }
    }))
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val radius = kernel.length / 2
    val neighbors = getNeighbors(image, radius)

    neighbors.map(row => row.map(neighbor => {
      neighbor.zip(kernel).map(e => e._1.zip(e._2).map(e => e._1 * e._2).sum).sum
    }))
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def combination(k: Integer,n: Integer): Integer = {
      if (k <= 0 || k >= n) {
        1
      }
      else {
        (combination(k, n-1) % m + combination(k - 1, n-1) % m) % m
      }
    }

    def pascalRow(n: Integer): List[Pixel] = {
        (0 until size).map(k =>
          if ( k <= n ) {
            funct(combination(k, n))
          }
          else {
            Pixel(0, 0, 0)
          }).toList
      }


    (0 until size).map(n => pascalRow(n)).toList
  }


}
