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
                    # 'scn_cot_002_light_shaman_step_ply', \
                    # 'scn_cot_003_light_shaman_step_ply_no_capture', \
                    # 'scn_cot_004_light_shaman_capture_ply', \
                    # 'scn_cot_005_light_shaman_capture_ply_passives', \
                    # 'scn_cot_006_dark_shaman_step_ply', \
                    # 'scn_cot_007_dark_shaman_step_ply_no_capture', \
                    # 'scn_cot_008_dark_shaman_capture_ply', \
                    # 'scn_cot_009_dark_shaman_capture_ply_passives', \
                    # 'scn_cot_010_activating_wave_step_field', \
                    # 'scn_cot_011_activating_wave_capture_field', \
                    # 'scn_cot_012_shaman_transparent_to_own_pieces', \
                    'scn_cot_013_shaman_not_transparent_to_opponents_pieces', \
                    # 'scn_cot_014_shaman_transparent_to_opponents_shaman', \
                ]
