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
                    # 'scn_o_17_starchild_activated_wave_not_teleporting_init', \
                    # 'scn_o_18_starchild_activated_wave_not_teleporting_end', \
                    'scn_o_16_starchild_activated_wave_not_moving_a_star', \
                    # 'scn_o_22_light_starchild_init_trance_journey', \
                    # 'scn_o_23_dark_starchild_init_trance_journey', \
                    # 'scn_o_24_shaman_init_trance_journey', \
                    # 'scn_o_25_trance_journey_started_by_shaman', \
                ]
