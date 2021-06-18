#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under 3-clause (modified) BSD license. See LICENSE for details.


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
                    # 'scn_hd_01_centaur_same_color', \
                    # 'scn_hd_02_centaur_opposite_color', \
                    # 'scn_hd_03_centaur_multi_step_init', \
                    # 'scn_hd_04_centaur_multi_step_second', \
                    # 'scn_hd_05_centaur_multi_step', \
                    'scn_hd_07_wave_activation_by_centaur_first_step', \
                    'scn_hd_08_wave_activation_by_centaur_second_step', \
                    'scn_hd_09_wave_activation_by_centaur_complete', \
                ]
