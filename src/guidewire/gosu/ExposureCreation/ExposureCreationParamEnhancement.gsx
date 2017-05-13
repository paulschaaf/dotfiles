package libraries
uses gw.api.database.IQueryBeanResult

enhancement ExposureCreationParamEnhancement : entity.ExposureCreationParam {

  // Find all instances
  static function findAll() : IQueryBeanResult<ExposureCreationParam> {
    var query = gw.api.database.Query.make(ExposureCreationParam)
    return query.select()
  }
  
  // Ask every instance to create an exposure if appropriate
  static function createAllMatchingExposuresFor(aClaim : Claim) : Exposure[] {
    return findAll()
      .toTypedArray()
      .map(\ param -> param.createExposureIfMatching(aClaim))
      .where(\ exp -> exp != null)
  }
  
  // If aClaim fits the pattern I describe, answer a new exposure, otherwise answer null
  function createExposureIfMatching(aClaim : Claim) : Exposure {
    var exp = this.matchesClaim(aClaim)
      ? aClaim.newExposure(this.CoverageType, this.CoverageSubtype, true)
      : null
    return exp
  }

  // What type of Incident do I refer to?
  property get IncidentType() : gw.lang.reflect.IType {
    return Type.forName("entity." + this.Incident.Code)
  }
  
  // Does this claim match the pattern of parameters I represent?
  function matchesClaim(aClaim : Claim) : boolean {
    var thisIncidentType  = this.IncidentType
    
    var lossTypeMatch     = this.LossType     == null OR this.LossType   == aClaim.LossType
    var lobTypeMatch      = this.LOBCode      == null OR this.LOBCode    == aClaim.LOBCode
    var policyTypeMatch   = this.PolicyType   == null OR this.PolicyType == aClaim.Policy.PolicyType
    var lossCauseMatch    = this.LossCause    == null OR this.LossCause  == aClaim.LossCause
      // Checking for null, though currently these fields are required
    var coverageTypeMatch = this.CoverageType == null OR aClaim.Policy.AllCoverages.hasMatch(\ cov -> this.CoverageType == cov.Type)
    var incidentMatch     = this.Incident     == null OR aClaim.Incidents.hasMatch(\ incd -> thisIncidentType.isAssignableFrom(incd.IntrinsicType))
    
    return lossTypeMatch and lobTypeMatch and policyTypeMatch and lossCauseMatch and coverageTypeMatch and incidentMatch
  }
  
}
