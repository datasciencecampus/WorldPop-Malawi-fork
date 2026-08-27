import pandas as pd
import geopandas as gpd

from pandas.testing import assert_frame_equal
from geopandas.testing import assert_geodataframe_equal

from pathlib import Path

if __name__ == "__main__":
    # set output directory
    output_dir = Path().cwd().joinpath("data")

    # import pre- and post- refactor outputs
    pre_refactor_dir = output_dir.joinpath("pre-refactor")
    post_refactor_dir = output_dir.joinpath("post-refactor")

    ### summarised survey data

    # load data
    pre_summary_df = pd.read_csv(
        pre_refactor_dir.joinpath("summarized_survey_data.csv")
    )
    post_summary_df = pd.read_csv(
        post_refactor_dir.joinpath("summarized_survey_data.csv")
    )

    df_comparison = assert_frame_equal(pre_summary_df, post_summary_df)

    if df_comparison is None:
        print(
            "Pre and Post Refactor versions of 'summarized_survey_data.csv' "
            "are exactly the same."
        )

    ### household count geopackage

    pre_hh_gpkg = gpd.read_file(pre_refactor_dir.joinpath("hh_size_data.gpkg"))
    post_hh_gpkg = gpd.read_file(post_refactor_dir.joinpath("hh_size_data.gpkg"))

    gpkg_comparison = assert_geodataframe_equal(pre_hh_gpkg, post_hh_gpkg)

    if df_comparison is None:
        print(
            "Pre and Post Refactor versions of 'hh_size_data.gpkg' "
            "are exactly the same."
        )
