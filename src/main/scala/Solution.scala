import util.MyUtil.{Sir, opSplit}
import util.Pixel
import util.Util.{getNeighbors, toGrayScale}

import scala.annotation.tailrec

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

    // Get the length of the image
    val length = withoutNewLine.drop(1).head.foldRight("")(_ + "" + _).split(" ")(0).toInt;

    // Get the list of pixels (as strings)
    val pixelsString = withoutNewLine.drop(3).map(list => list.fold("")(_ + "" + _)).map(e => e.toString);
    // Get the list of pixels (as Pixel objects)
    val pixelList = pixelsString.map(e => e.split(" ")).map(e => Pixel(e(0).toInt, e(1).toInt, e(2).toInt));
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
    @tailrec
    def pascalTriangle(n: Int, acc: List[List[Int]]): List[List[Int]] = {
      if (n == 0) {
        acc
      } else {
        val precRow = acc.last
        val newRow = precRow.zip(precRow.tail).map(e => (e._1 + e._2) % m)
        pascalTriangle(n - 1, acc :+ (1 +: newRow :+ 1))
      }
    }

    /*
    * Create a list of black pixels
    * @param n the number of black pixels to create
    * @return a list of black pixels
     */
    def blackListPixels(n: Int): List[Pixel] = {
      @tailrec
      def blackListPixelsRec(n: Int, acc: List[Pixel]): List[Pixel] = {
        if (n == 0) {
          acc
        } else {
          blackListPixelsRec(n - 1, Pixel(0, 0, 0) :: acc)
        }
      }
      blackListPixelsRec(n, Nil)
    }

    // get the pascal triangle as a list of lists of integers
    val intPascal = pascalTriangle(size - 1, List(List(1)))

    // transform the pascal triangle into a list of lists of pixels
    val pixelPascal = intPascal.map(list => list.map(e => funct(e)))

    // complete the pascal triangle with black pixels
    val completePascal = pixelPascal.map(list => list ++ blackListPixels(size - list.length));

    completePascal
  }


}
