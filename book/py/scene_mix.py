#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
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


class SceneMix(SceneMixin, \
               SceneCroatianTiesMixin, \
               SceneMayanAscendancyMixin, \
               SceneAgeOfAquariusMixin, \
               SceneMirandasVeilMixin, \
               SceneNineteenMixin, \
               SceneHemerasDawnMixin, \
               SceneTamoanchanRevisitedMixin, \
               SceneConquestOfTlalocanMixin, \
               SceneDiscoveryMixin, \
               SceneOneMixin):

    # overrides
    def _get_recent_scene_method_names(self):
        return  [
                    # 'scn_cot_012_shaman_transparent_to_own_pieces', \
                    # 'scn_cot_013_shaman_not_transparent_to_opponents_pieces', \
                    # 'scn_cot_014_shaman_transparent_to_opponents_shaman', \
                    # 'scn_cot_015_shaman_transparency_not_heeded_while_capturing', \
                    # 'scn_mv_09_wave_no_block_castling_king', \
                    # 'scn_mv_10_wave_no_block_castling_rook', \
                    # 'scn_mv_11_wave_block_castling_rook', \
                    'scn_n_23_new_castling_init', \
                    'scn_n_24_new_castling_end', \
                ]
