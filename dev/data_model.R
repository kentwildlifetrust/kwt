file.remove("dev/data_model.json")
db_to_json_data_model(conn, "dev/data_model.json")
m <- postgisMoR::read_json_data_model("dev/data_model.json")
m <- postgisMoR::model_relational_data(m)


file.remove("dev/data_model_.json")
postgisMoR::write_json_data_model(m, "dev/data_model_.json")


m <- postgisMoR::read_json_data_model("test.json")
m <- postgisMoR::model_relational_data(m)
file.remove("test.json")
postgisMoR::write_json_data_model(m, "test.json")


