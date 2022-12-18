fake_masterdata_debt_datastore <-
  function(id = "8",
           id_name = "credit_parent_ar_company_id",
           ald_sector = "Oil&Gas",
           ald_location = "DE",
           technology = "Gas",
           year = 2022,
           country_of_domicile = "DE",
           ald_production_unit = "GJ",
           ald_production = 5.228123e+07,
           ald_emissions_factor_unit = "tonnes of CO2 per GJ",
           ald_emissions_factor = 0.06202439) {
    data.frame(
      id = id,
      id_name = id_name,
      ald_sector = ald_sector,
      ald_location = ald_location,
      technology = technology,
      year = year,
      country_of_domicile = country_of_domicile,
      ald_production_unit = ald_production_unit,
      ald_production = ald_production,
      ald_emissions_factor_unit = ald_emissions_factor_unit,
      ald_emissions_factor = ald_emissions_factor
    )
  }
