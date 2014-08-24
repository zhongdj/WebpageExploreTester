package net.imadz.web.explorer.utils

/**
 * Created by Scala on 14-8-24.
 */

/**
 * Http status code, please refer below link:
 * http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
 */
object HttpResponseUtil {
  def getErrorMessage(responseCode: Int): String = {
    responseCode match {
      case 400 => "Bad Request"
      case 401 => "Unauthorized"
      case 403 => "Forbidden"
      case 404 => "Not Found"
      case 405 => "Method Not Allowed"
      case 406 => "Not Acceptable"
      case 407 => "Proxy Authentication Required"
      case 408 => "Request Timeout"
      case 409 => "Conflict"
      case 410 => "Gone"
      case 411 => "Length Required"
      case 412 => "Precondition Failed"
      case 413 => "Request Entity Too Large"
      case 414 => "Request-URI Too Long"
      case 415 => "Unsupported Media Type"
      case 416 => "Requested Range Not Satisfiable"
      case 417 => "Expectation Failed"
      case 500 => "Internal Server Error"
      case 501 => "Not Implemented"
      case 502 => "Bad Gateway"
      case 503 => "Service Unavailable"
      case 504 => "Gateway Timeout"
      case 505 => "HTTP Version Not Supported"
    }
  }
}
