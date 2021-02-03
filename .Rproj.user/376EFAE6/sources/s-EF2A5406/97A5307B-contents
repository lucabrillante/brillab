#' Expression library for plotting
#'
#'This is a list of common measurement units with special characters that are useful
#'when passing labels to plots.
#' @param what specify the expression you want. Use list_all to print the options to console
#'
exp_lib <- function(what){
  if(what == "delta13C_lab")       return(expression(delta~"\u00b9\u00b3C"))
  if(what == "delta13C_unit")      return(expression("\u2030"))
  if(what == "delta13C_lab_unit")  return(expression(delta~"\u00b9\u00b3C"~"(\u2030)"))
  if(what == "psiStem_lab")        return(expression(Psi[stem]))
  if(what == "psiStem_lab_unit")   return(expression(Psi[stem]~"(MPa)"))
  if(what == "gs_lab")             return(expression(italic(g)[s]))
  if(what == "gs_unit")            return(expression(mmol~H[2]*O~m^{-2}~s^{-1}))
  if(what == "gs_lab_unit")        return(expression(italic(g)[s]~"("*mmol~H[2]*O~m^{-2}~s^{-1}*")"))
  if(what == "gs_unit_mol")        return(expression(mol~H[2]*O~m^{-2}~s^{-1}))
  if(what == "gs_lab_unit_mol")    return(expression(italic(g)[s]~"("*mol~H[2]*O~m^{-2}~s^{-1}*")"))
  if(what == "an_lab")             return(expression(italic(A)[N]))
  if(what == "an_unit")            return(expression(mu*mol~CO[2]~m^2~s^-1))
  if(what == "an_lab_unit")        return(expression(italic(A)[N]~"("*mu*mol~CO[2]~m^2~s^-1*")"))
  if(what == "wuei_unit")          return(expression(mu*mol~CO[2]~mmol^{-1}~H[2]*O))
  if(what == "wuei_lab_unit")      return(expression(WUEi~"("*mu*mol~CO[2]~mmol^{-1}~H[2]*O*")"))
  if(what == "co2r")               return(expression(CO[2]~"("*mu*mol~mol^-1*")"))
  if(what == "er_unit")            return(expression(ER~Omega~m^{-1}))
  if(what == "temp_c")             return(expression("Temperature (\u00B0C)"))

  if(what == "list_all") print(c("delta13C_lab", "delta13C_unit", "delta13C_lab_unit",
                               "psiStem_lab", "psiStem_lab_unit", "gs_lab",
                               "gs_unit", "gs_lab_unit", "gs_unit_mol", "gs_lab_unit_mol",
                               "an_lab", "an_unit", "an_lab_unit", "wuei_unit",
                               "wuei_lab_unit", "co2r", "er_unit", "temp_c"))
}
