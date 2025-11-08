#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_classical_chess import SceneClassicalChessMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin
from scene_simple import SceneSimpleMixin


class SceneMix( SceneMixin, \
                SceneClassicalChessMixin, \
                SceneCroatianTiesMixin, \
                SceneMayanAscendancyMixin, \
                SceneAgeOfAquariusMixin, \
                SceneMirandasVeilMixin, \
                SceneNineteenMixin, \
                SceneHemerasDawnMixin, \
                SceneTamoanchanRevisitedMixin, \
                SceneConquestOfTlalocanMixin, \
                SceneDiscoveryMixin, \
                SceneOneMixin, \
                SceneSimpleMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    # 'scn_ct_01_pegasus_initial', \
                    # 'scn_ma_19_pyramid_vs_king', \
                    # 'scn_ma_20_pyramid_vs_bishop', \
                    # 'scn_aoa_01_unicorn_same_color', \
                    # 'scn_mv_013_wave_no_block_castling_king', \
                    # 'scn_mv_014_wave_no_block_castling_rook', \
                    # 'scn_mv_015_wave_block_castling_rook', \
                    # 'scn_mv_029_wave_same_color', \
                    'scn_mv_030_wave_opposite_color', \
                ]
